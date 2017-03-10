defmodule TypedElixir do
  @moduledoc """
  """


  alias TypedElixir.HMEnv
  alias TypedElixir.Type




  # defmodule TypeGeneric       ,do: defstruct []
  # defmodule TypeConst         ,do: defstruct type: :"$$NO$TYPE$$", meta: %{}
  # defmodule TypeCall          ,do: defstruct args: nil, return: %TypeGeneric{} # blah(args)
  # defmodule TypeCallIndirect  ,do: defstruct args: nil, return: %TypeGeneric{} # blah.(args)
  # defmodule TypeModule        ,do: defstruct []
  # defmodule TypePtr do
  #   defstruct ptr: -1
  #   def new(env, subtype \\ %TypeGeneric{}) do
  #     {env, ptr} = HMEnv.new_counter(env, :ptr)
  #     type = %TypePtr{ptr: ptr}
  #     env = HMEnv.push_type_ptr(env, ptr, subtype)
  #     {env, type}
  #   end
  # end






  defmacro __using__(_opts) do
    quote bind_quoted: [] do
      import TypedElixir, only: [defmodulet: 2]
    end
  end


  defp debug(val, opts, section, prefix \\ nil) do
    debugOpts = opts[:debug] || []
    isEnabled = true === debugOpts || section in debugOpts || :all in debugOpts
    if isEnabled do
      if prefix do
        IO.inspect({section, prefix, val})
        val
      else
        IO.inspect({section, val})
        val
      end
    else
      val
    end
  end


  defmacro defmodulet(alias_name, opts) when is_list(opts) do
    block_module = opts[:do]
    opts = Keyword.delete(opts, :do)
    opts = Keyword.put(opts, :environment, __CALLER__)
# opts = [debug: true] ++ opts
    debug(:defmodulet, opts, :defmodulet)

    alias_name = Macro.expand(alias_name, __CALLER__)
      |> debug(opts, :ALIAS)

    block_module = block_module
      |> debug(opts, :AST)

    block_module = wrap_with_block_if_not(block_module)
      |> debug(opts, :BLOCK)

    block_module = Macro.expand(block_module, __CALLER__)
      |> debug(opts, :AST_EXPANDED)

    env = %HMEnv{} |> HMEnv.push_scope(:top)
      |> debug(opts, :ENV)

    {env, block_module} = typecheck_module(env, opts, alias_name, block_module)
      |> debug(opts, :TYPECHECKED)

    {_env, _scope} = env |> HMEnv.pop_scope(:top)
      |> debug(opts, :FINAL_ENV)

    quote do
      defmodule unquote(alias_name) do
        unquote(block_module)
      end
    end
  end



  defp wrap_with_block_if_not(nil), do: nil
  defp wrap_with_block_if_not({:__block__, _, _} = block), do: block
  defp wrap_with_block_if_not([{:__block__, _, _} = block]), do: block
  defp wrap_with_block_if_not(not_block), do: {:__block__, [], [not_block]}



  defp typecheck_module(env, opts, name, body)
  defp typecheck_module(env, _opts, name, nil) when is_atom(name), do: {env, nil}
  defp typecheck_module(env, opts, name, {:__block__, meta, body_asts}) when is_atom(name) do
    # environment = opts[:environment]
    env = HMEnv.push_scope(env, :module, name)
    {env, bodies} = Enum.reduce(body_asts, {env, []}, fn(module_entry_ast, {env, bodies}) ->
      # module_entry_ast = Macro.expand(module_entry_ast, environment)
      {env, body} = type_check_body(env, opts, module_entry_ast)
      {env, [body | bodies]}
    end)
    bodies = :lists.reverse(bodies)
    Enum.map(bodies, &debug(&1, opts, :MODULE_BODY, name))
    # TODO:  Parse out the module information to make a type-map in a callback function
    {env, _scope} = HMEnv.pop_scope(env, :module)
    {env, type} = Type.Module.new(env, %{})
    ast = {:__block__, [type: type] ++ meta, bodies}
    {env, ast}
  end



  defp type_check_body(env, opts, module_entry_ast)
  defp type_check_body(env, opts, {:def, meta, [head_ast, body_ast]}) do
    environment = opts[:environment]
    head_ast = Macro.expand(head_ast, environment)
    body_ast = Macro.expand(body_ast, environment)
    {env, head} = type_check_def_head(env, opts, head_ast) # This will push scope with the name
    {env, body} = type_check_def_body(env, opts, body_ast)
    {env, func_type} = get_type_of(env, head)
    {env, return_type} = get_type_of(env, body)
    # {env, return_type} = unify_types!(env, func_type.return, return_type)
    # resolve_types!(env, return_type, func_type.return_type) # Can the return_type be returned by the func returns?
    {env, _scope} = HMEnv.pop_scope(env, :def)
    env = resolve_fun_return_type(env, opts, return_type, func_type.return_type)
    # func_type = %{func_type | return_type: return_type}
    ast = {:def, [type: func_type] ++ meta, [head, body]}
    {env, ast}
  end
  defp type_check_body(env, opts, {:@, attrMeta, [attr_ast]}) do
    {env, attr} = type_check_attribute(env, opts, attr_ast)
    ast = {:@, attrMeta, [attr]}
    {env, ast}
  end
  defp type_check_body(_env, _opts, module_entry_ast) do
    throw {:TODO_UNHANDLED_BODY_AST, module_entry_ast}
  end



  defp resolve_fun_return_type(env, opts, orig_return_type, orig_typed_return_type) do
    {env, return_type} = Type.get_type_or_ptr_type(env, orig_return_type)
    {env, typed_return_type} = Type.get_type_or_ptr_type(env, orig_typed_return_type)
    {env, type} = resolve_fun_return_type_(env, opts, return_type, typed_return_type)
    {env, subtype} = Type.Ptr.Link.new(env, type)
    env = case orig_typed_return_type do
      %Type.Ptr{} -> Type.Ptr.set(env, orig_typed_return_type, subtype)
      _ -> env
    end
    env
  end
  defp resolve_fun_return_type_(env, _opts, return_type, typed_return_type)
  # Uhh, unresolved return, so it is never set to anything but itself...
  defp resolve_fun_return_type_(env, _opts, %Type.Ptr.Unbound{id: id}, %Type.Ptr.Unbound{id: id}) do
    Type.Const.new(env, :no_return)
  end
  defp resolve_fun_return_type_(env, _opts, type, type), do: {env, type}
  # The typed return is not set yet, so set it to the body expression
  defp resolve_fun_return_type_(env, _opts, return_type, %Type.Ptr.Unbound{}) do
    {env, return_type}
  end
  defp resolve_fun_return_type_(env, _opts, return_type, %Type.Ptr.Generic{named: false}) do
    {env, return_type}
  end
  defp resolve_fun_return_type_(env, _opts, return_type, typed_return_type) do
    {env, type} = resolve_types!(env, return_type, typed_return_type)
throw {:TODO_RESOLVE_FUN_RETURN_TYPE_UNKNOWN, type, :<, return_type, typed_return_type}
    {env, typed_return_type}
  end



  defp type_check_attribute(env, opts, attr_ast)
  defp type_check_attribute(env, opts, {:spec, specMeta, [spec_ast]}) do
    {env, type} = parse_type_from_specattr(env, opts, spec_ast)
    ast = {:spec, [type: type] ++ specMeta, [spec_ast]}
    {env, ast}
  end
  defp type_check_attribute(env, opts, {:type, specMeta, [spec_ast]}) do
    {env, type} = parse_type_from_typeattr(env, opts, spec_ast)
    ast = {:type, [type: type] ++ specMeta, [spec_ast]}
    {env, ast}
  end
  defp type_check_attribute(env, _opts, {:moduledoc, _, _}=attr_ast) do
    {env, attr_ast}
  end
  defp type_check_attribute(env, _opts, {:doc, _, _}=attr_ast) do
    {env, attr_ast}
  end
  defp type_check_attribute(env, _opts, attr_ast) do
    IO.inspect {:UNKNOWN_ATTRIBUTE, attr_ast}
    {env, attr_ast}
  end



  defp type_check_def_body(env, opts, body_ast)
  defp type_check_def_body(env, opts, [do: expression_or_block]) do
    environment = opts[:environment]
    expression_or_block = Macro.expand(expression_or_block, environment)
    {env, do_ast} = type_check_expression(env, opts, expression_or_block)
    ast = [do: do_ast]
    {env, ast}
  end
  defp type_check_def_body(_env, _opts, body_ast) do
    throw {:TODO_DEF_BODY, body_ast}
  end



  defp type_check_expression(env, opts, expression_or_block)
  # General constants
  defp type_check_expression(env, _opts, const)
    when is_atom(const)
    when is_integer(const)
    when is_float(const), do: {env, const}
  # Bindings
  defp type_check_expression(env, _opts, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
    key = {:binding, name}
    case HMEnv.get_type(env, key) do
      nil -> throw {:BINDING_NOT_FOUND, name}
      type ->
        ast = {name, [type: type] ++ meta, scope}
        {env, ast}
    end
  end
  # Special constructs in Elixir
  defp type_check_expression(env, opts, {:=, bindMeta, [binding_ast, expr_ast]}) do
    environment = opts[:environment]
    binding_ast = Macro.expand(binding_ast, environment)
    expr_ast = Macro.expand(expr_ast, environment)
    {env, expr} = type_check_expression(env, opts, expr_ast)
    {env, exprType} = get_type_of(env, expr)
    {env, binding} = type_check_binding(env, opts, binding_ast, exprType)
    {env, bindingType} = get_type_of(env, binding)
    case resolve_types!(env, exprType, bindingType) do
      {_env, %Type.Const{const: :no_return}} -> # Do not allow :no_return as a return type, because it will never happen..
        throw {:INVALID_ASSIGNMENT_NOT_ALLOWED, :no_return}
      {env, type} ->
        ast = {:=, [type: type] ++ bindMeta, [binding, expr]}
        {env, ast}
    end
  end
  # Direct function call
  defp type_check_expression(env, opts, {name, meta, args_ast}) when is_atom(name) and is_list(args_ast) do
    environment = opts[:environment]
    args_ast = Enum.map(args_ast, &Macro.expand(&1, environment))
    arity = length(args_ast)
    key = {:function, name, arity}
    {env, args} = HMEnv.map_env(env, args_ast, &type_check_expression(&1, opts, &2))
    {env, args_types} = HMEnv.map_env(env, args, &get_type_of/2)
    case HMEnv.get_type(env, key) do
      nil ->
        throw {:FUNCTION_NOT_FOUND, name, arity, env.types}
      %Type.Func{args_types: call_args, return_type: return_type} when length(call_args)===arity ->
        state = %{
          return_type: return_type,
          ids: %{}
        }
        state = Enum.zip(args_types, call_args) |> Enum.reduce(state, fn({fromType, intoType}, state) ->
          {_, base_return_type} = Type.get_type_or_ptr_type(env, state.return_type)
          case Type.get_type_or_ptr_type(env, intoType) do
            {_env, %Type.Ptr.Generic{named: false}} -> # If putting into an unnamed generic, it always works
              state
            {_env, %Type.Ptr.Generic{id: id, named: true}=t} -> # Else if named generic, make sure same type as a prior used one if another
              state =
                if t === base_return_type do
                  %{state | return_type: fromType}
                else
                  state
                end
              case state[id] do
                nil ->
                  ids = Map.put_new(state.ids, id, fromType)
                  %{state | ids: ids}
                priorFromType ->
                  unify_types!(env, priorFromType, fromType)
                  state
              end
            {_env, _type} ->
              resolve_types!(env, fromType, intoType)
              state
          end
          # unify_types!(env, fromType, intoType)
          # resolve_types!(env, fromType, intoType)
        end)
        ast = {name, [type: state.return_type] ++ meta, args}
        {env, ast}
      invalidType -> throw {:DEF_HEAD_DEFNAME, "Invalid type for name", name, meta, args, invalidType, env}
    end
  end
  defp type_check_expression(_env, _opts, expression_or_block) do
    throw {:TODO_EXPRESSION, expression_or_block}
  end



  defp type_check_def_head(env, opts, head_ast)
  defp type_check_def_head(env, opts, {:when, whenMeta, [{name, _meta, _args_ast}=head_ast, when_ast]}) when is_atom(name) do
    {env, head} = type_check_def_head_when(env, opts, head_ast, when_ast)
    ast = {:when, whenMeta, [head, when_ast]}
    {env, ast}
  end
  defp type_check_def_head(env, opts, {name, _meta, _args_ast}=head_ast) when is_atom(name) do
    type_check_def_head_when(env, opts, head_ast, nil)
  end



  defp type_check_def_head_when(env, opts, head_ast, when_ast)
  defp type_check_def_head_when(env, opts, {name, meta, args_ast}, when_ast) do
    arg_count = length(args_ast)
    key = {:function, name, arg_count}
    case HMEnv.get_type(env, key) do
      nil -> # No pre-defined type for this fun-name, use the inferred value
        {env, args} = type_check_fun_bindings(env, opts, args_ast)
        {env, args} = type_check_fun_when(env, opts, args, when_ast)
        {env, args_types} = HMEnv.map_env(env, args, &get_type_of/2)
        {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
        {env, type} = Type.Func.new(env, args_types, return_type, false)
        # Put the head type into the scope for calling it recursively
        env = HMEnv.push_type(env, {:function, name, arg_count}, type)
        # Make this function available at the global module scope before pushing a new scope
        env = HMEnv.push_scope(env, :def, name)
        ast = {name, [type: type] ++ meta, args}
        {env, ast}
      %Type.Func{args_types: args_types} = type when length(args_types)===arg_count ->
        {env, args} = type_check_fun_bindings(env, opts, args_ast, args_types)
        {env, args} = type_check_fun_when(env, opts, args, when_ast)
        {env, args_types} = HMEnv.map_env(env, args, &get_type_of/2)
        {env, _} = HMEnv.zipmap_env(env, args_types, args_types, &resolve_types!/3)
        # Put the head type into the scope for calling it recursively
        env = HMEnv.push_type(env, {:function, name, arg_count}, type)
        # Make this function available at the global module scope before pushing a new scope
        env = HMEnv.push_scope(env, :def, name)
        ast = {name, [type: type] ++ meta, args}
        {env, ast}
      invalidType -> throw {:DEF_HEAD_DEFNAME, "Invalid type for name", name, meta, args_ast, invalidType, env}
    end
  end



  defp type_check_fun_when(env, opts, args, when_ast)
  # There is no :when condition
  defp type_check_fun_when(env, _opts, args, nil) do
    {env, args}
  end
  # There are no args, but there is a :when condition, need to handle that in case it is closed over
  defp type_check_fun_when(env, _opts, [], when_ast) do
    throw {:TODO_REFINE_FUNC_WHEN_NO_ARGS, when_ast, env}
  end
  # There is a when condition with args
  defp type_check_fun_when(env, _opts, args, when_ast) do
    throw {:TODO_REFINE_ARGS_WITH_WHEN, args, when_ast, env}
  end



  defp type_check_fun_bindings(env, opts, args_ast)
  defp type_check_fun_bindings(env, opts, args_ast) do
    {env, args} = Enum.reduce(args_ast, {env, []}, fn(arg_ast, {env, prior}) ->
      {env, type} = Type.Ptr.Generic.new_ptr(env, false)
      {env, arg} = type_check_binding(env, opts, arg_ast, type)
      {env, prior ++ [arg]}
    end)
    {env, args}
  end
  defp type_check_fun_bindings(env, opts, args_ast, argTypes) do
    {env, args} = Enum.zip(args_ast, argTypes) |> Enum.reduce({env, []}, fn({arg_ast, arg_type}, {env, prior}) ->
      {env, arg} = type_check_binding(env, opts, arg_ast, arg_type)
      {env, prior ++ [arg]}
    end)
    {env, args}
  end



  # defp type_check_binding(env, opts, binding, type \\ nil)
  defp type_check_binding(env, _opts, {name, meta, scope}, type) when is_atom(name) and is_atom(scope) do
    {env, type} = case type do
      nil ->
        Type.Ptr.Unbound.new_ptr(env)
      type ->
        Type.Ptr.Link.new_ptr(env, type)
    end
    key = {:binding, name}
    env = HMEnv.push_type(env, key, type)
    ast = {name, [type: type] ++ meta, scope}
    {env, ast}
  end
  defp type_check_binding(_env, _opts, binding, type) do
    throw {:TODO_BINDING, binding, type}
  end


  # defp infer_type_from_funhead





  defp parse_type_from_typespec(env, opts, typespec_ast)
  defp parse_type_from_typespec(env, _opts, {:any, _meta, []}) do
    {env, type} = Type.Ptr.Generic.new_ptr(env, true)
    {env, type}
  end



  defp parse_type_from_typeattr(env, opts, type_ast)
  # A typespec that does not take an argument to refine the type
  defp parse_type_from_typeattr(env, opts, {:::, _meta, [{typename, _nameMeta, scope}, typespec_ast]}) when is_atom(scope) do
    {env, type} = parse_type_from_typespec(env, opts, typespec_ast)
    # Name the type for later use
    key = {:typename, typename}
    env = HMEnv.push_type(env, key, type)
    {env, type}
  end



  defp parse_type_from_specattr(env, opts, spec_ast)
  defp parse_type_from_specattr(env, opts, {:::, _meta, [{name, _nameMeta, args_ast}, return_ast]}) when is_atom(name) do
    argCount = length(args_ast)
    {env, args_types} = HMEnv.map_env(env, args_ast, &parse_type_from(&1, opts, &2))
    {env, returnType} = parse_type_from(env, opts, return_ast)
    {env, type} = Type.Func.new(env, args_types, returnType, false)
    env = HMEnv.push_type(env, {:function, name, argCount}, type)
    {env, type}
  end




  # defp parse_type_from_specattr_arg(env, opts, arg_ast)
  # defp parse_type_from_specattr_arg(env, opts, {typename, _meta, scope}) when is_atom(typename) and is_atom(scope) do
  #   case HMEnv.get_type(env, {:typename, typename}) do
  #     nil -> throw {:TYPENAME_DOES_NOT_EXIST, typename}
  #     type -> {env, type}
  #   end
  # end
  # defp parse_type_from_specattr_arg(env, opts, arg_ast) do
  #   # Use parse_type_from on each type?
  #   throw {:TODO_PARSE_SPEC_ARG, arg_ast}
  # end

  defp parse_type_from(env, opts, type_ast)
  # Constants
  defp parse_type_from(env, _opts, atom) when is_atom(atom)    ,do: Type.Const.new(env, :atom, values: [atom])
  defp parse_type_from(env, _opts, int) when is_integer(int)   ,do: Type.Const.new(env, :integer, values: [int])
  defp parse_type_from(env, _opts, float) when is_float(float) ,do: Type.Const.new(env, :float, values: [float])
  defp parse_type_from(env, _opts, {:atom, _meta, []})         ,do: Type.Const.new(env, :atom)
  defp parse_type_from(env, _opts, {:integer, _meta, []})      ,do: Type.Const.new(env, :integer)
  defp parse_type_from(env, _opts, {:float, _meta, []})        ,do: Type.Const.new(env, :float)
  defp parse_type_from(env, _opts, {:any, _meta, []})          ,do: Type.Ptr.Generic.new_ptr(env, false)
  # Variable name lookups
  defp parse_type_from(env, _opts, {typename, _meta, scope}) when is_atom(typename) and is_atom(scope) do
    case HMEnv.get_type(env, {:typename, typename}) do
      nil -> throw {:TYPENAME_DOES_NOT_EXIST, typename}
      type -> {env, type}
    end
  end
  defp parse_type_from(_env, _opts, type_ast) do
    throw {:TODO_PARSE_TYPE, type_ast}
  end




  # Get type of
  defp get_type_of(env, thing)
  defp get_type_of(env, atom)    when is_atom(atom)       ,do: Type.Const.new(env, :atom, values: [atom])
  defp get_type_of(env, integer) when is_integer(integer) ,do: Type.Const.new(env, :integer, values: [integer])
  defp get_type_of(env, float)   when is_float(float)     ,do: Type.Const.new(env, :float, values: [float])
  defp get_type_of(env, [do: do_ast])                     ,do: get_type_of(env, do_ast)
  defp get_type_of(env, {_, meta, _})                     ,do: {env, Keyword.fetch!(meta, :type)}






  # Resolve a type to a type
  def resolve_types!(env, fromType, intoType) do
    inspect {:RESOLVERINATING, env, fromType, intoType}
    type = resolve_types(env, fromType, intoType)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end


  def resolve_types(env, fromType, intoType) do
    {env, fromType} = Type.get_type_or_ptr_type(env, fromType)
    {env, intoType} = Type.get_type_or_ptr_type(env, intoType)
    resolve_types_nolinks(env, fromType, intoType)
  end


  defp resolve_types_nolinks(env, fromType, intoType)
  defp resolve_types_nolinks(env, type, type), do: {env, type}
  defp resolve_types_nolinks(env, fromType, %Type.Ptr.Unbound{}=_intoType), do: {env, fromType} # Everything goes in to an Unbound
  defp resolve_types_nolinks(env, fromType, %Type.Ptr.Generic{named: false}=_intoType), do: {env, fromType} # Everything goes out to an unnamed generic too (good luck recovering it)
  # Keep TypePtr handling last
  # def resolve_types_nolinks(env, %Type.Ptr{ptr: fromPtr}=from, %Type.Ptr{ptr: intoPtr}=into) do
  #   intoType = HMEnv.get_type_ptr(env, intoPtr)
  #   intoPath = resolve_types_nolinks(env, from, intoType)
  #   if Exception.exception?(intoPath) do
  #     fromType = HMEnv.get_type_ptr(env, fromPtr)
  #     resolve_types_nolinks(env, fromType, into)
  #   else
  #     intoPath
  #   end
  # end
  # defp resolve_types_nolinks(env, %Type.Ptr{}=fromType, %Type.Ptr{}=intoType) do
  #   p0 = Type.Ptr.get(env, fromType)
  #   p1 = Type.Ptr.get(env, intoType)
  #   case {p0, p1} do
  #     {p0, %Type.Ptr.Unbound{}} -> {env, p0}
  #     # {%Type.Ptr.Unbound{id: id}, %Type.Ptr.Unbound{id: id}} -> p0
  #     # {%Type.Ptr.Generic{id: id0}, %Type.Ptr.Unbound{id: id1}} -> p0
  #     # {%Type.Ptr.Unbound{id: fromID}, %Type.Ptr.Unbound{id: intoID}} ->
  #     _ -> throw {:TODO_RESOLVE_2_PTRS, p0, p1}
  #   end
  # end
  # defp resolve_types_nolinks(env, fromType, %Type.Ptr{}=intoType) do
  #   case Type.Ptr.get(env, intoType) do
  #     %Type.Ptr.Unbound{} -> {env, fromType}
  #     %Type.Ptr.Generic{} -> throw {:CANNOT_RESOLVE_SPECIFIC_TYPE_TO_GENERIC, fromType}
  #   end
  # end
  # defp resolve_types_nolinks(env, fromType, %Type.Ptr{ptr: ptr}) do
  #   # intoType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # defp resolve_types_nolinks(env, %Type.Ptr{ptr: ptr}, intoType) do
  #   # fromType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # Catch-all
  defp resolve_types_nolinks(env, fromType, intoType) do
    {env, from} = Type.get_type_or_ptr_type(env, fromType)
    {_env, into} = Type.get_type_or_ptr_type(env, intoType)
    throw {:NO_TYPE_RESOLUTION, from, into}
  end



  # Unify two types
  def unify_types!(env, t0, t1) do
    type = unify_types(env, t0, t1)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end


  def unify_types(env, t0, t1) do
    {env, t0} = Type.get_type_or_ptr_type(env, t0)
    {env, t1} = Type.get_type_or_ptr_type(env, t1)
    unify_types_nolinks(env, t0, t1)
  end


  def unify_types_nolinks(env, t0, t1)
  def unify_types_nolinks(env, t, t), do: {env, t}
  def unify_types_nolinks(_env, %Type.Ptr.Generic{id: id0, named: true}=t0, %Type.Ptr.Generic{id: id1, named: true}=t1) when id0 !== id1 do
    throw {:NO_TYPE_UNIFICATION, :GENERICS_DO_NOT_MATCH, t0, t1}
  end
  def unify_types_nolinks(_env, t0, %Type.Ptr.Generic{named: false}), do: t0
  def unify_types_nolinks(_env, %Type.Ptr.Generic{named: false}, t1), do: t1
  # Keep TypePtr handling last
  # def unify_types_nolinks(env, %Type.Ptr{}=t0, %Type.Ptr{}=t1) do
  #   type0 = Type.Ptr.get(env, t1)
  #   path0 = unify_types_nolinks(env, t1, type0)
  #   if Exception.exception?(path0) do
  #     type1 = Type.Ptr.get(env, t0)
  #     unify_types_nolinks(env, type1, t0)
  #   else
  #     path0
  #   end
  # end
  # def unify_types_nolinks(env, t0, %Type.Ptr{ptr: ptr}) do
  #   # t1 = HMEnv.get_type_ptr(env, ptr)
  #   # unify_types_nolinks(env, t0, t1)
  # end
  # def unify_types_nolinks(env, %Type.Ptr{ptr: ptr}, t1) do
  #   # t0 = HMEnv.get_type_ptr(env, ptr)
  #   # unify_types_nolinks(env, t0, t1)
  # end
  # Catch-all
  def unify_types_nolinks(env, t0, t1) do
    {env, t0} = Type.get_type_or_ptr_type(env, t0)
    {_env, t1} = Type.get_type_or_ptr_type(env, t1)
    throw {:NO_TYPE_UNIFICATION, :NO_PATH, t0, t1}
  end





  # TODO:  Make sure the return value of a function that ends in `?` only
  # returns a boolean()

  # TODO:  Walk the call path of the function and see what it returns and make
  # sure it matches the spec




  # The below is too convoluted, doing the typing in-place, though that means
  # that all constructs must be known before used instead of just building it
  # up, I'm okay with that.  :-)


#   @fun_types [:def, :defp, :defmacro, :defmacrop]
#
#
#   defmacro __using__(_opts) do
#     quote bind_quoted: [] do
#       import TypedElixir, only: [defmodulet: 2]
#     end
#   end
#
#   defmacro defmodulet(alias_name, do: block_module) do
#     quote do
#       defmodule unquote(alias_name) do
#         @after_compile TypedElixir
#
#         unquote(clear_unneeded_specs(block_module))
#
#         TypedElixir.type_check(
#           unquote(alias_name),
#           unquote(Macro.escape(block_module)),
#           __ENV__
#           )
#       end
#     end
#   end

#   # defmodulet helpers
#
#   defp clear_unneeded_specs(block_module)
#   defp clear_unneeded_specs({:__block__, blockattrs, blockbody}) do
#     {:__block__, blockattrs, clear_unneeded_specs_module(blockbody)}
#   end
#
#   defp clear_unneeded_specs_module(blockbody)
#   defp clear_unneeded_specs_module([]), do: []
#   defp clear_unneeded_specs_module([{funtype, funattrs, funbody}|blockbody]) when funtype in @fun_types do
#     [{funtype, funattrs, clean_unneeded_specs_fun(funbody)} | clear_unneeded_specs_module(blockbody)]
#   end
#   defp clear_unneeded_specs_module([block|blockbody]) do
#     [block | clear_unneeded_specs_module(blockbody)]
#   end
#
#   defp clean_unneeded_specs_fun(block_fun)
#   defp clean_unneeded_specs_fun(block_fun) do
#     Macro.prewalk(block_fun, fn
#       {:@, _, [{:spec, _, _}]} -> nil
#       {:@, _, [{:spec, _, _}]} -> nil
#       ast -> ast
#     end)
#   end
#
#   # Type checking
#
#   def type_check(module_name, module_body, module_env)
#   def type_check(module_name, body, env) do
#     body = Macro.expand(body, env)
#     IO.puts "Type Checking: #{inspect module_name}"
#     # IO.inspect {module_name, body}#, env}
#     # empty_context = %{
#     #   types: %{},
#     #   specs: %{}
#     # }
#     # type_check_module(module_name, body, env, empty_context)
#
#     types = type_check_gather_types(module_name, body, env)
#     IO.puts "\nTypes:"
#     IO.inspect types
#
#     specs = type_check_gather_specs(module_name, body, env)
#     IO.puts "\nSpecs:"
#     IO.inspect specs
#
#     funs = type_check_gather_funs(module_name, body, env)
#     IO.puts "\nFuns:"
#     IO.inspect funs
#
#     # check_fun_follows_spec_ = check_fun_follows_spec(module_name, body, env)
#     # IO.puts "\ncheck_fun_follows_spec:"
#     # IO.inspect check_fun_follows_spec_
#
#     {type_fun_warnings, type_fun_errors} = check_types(module_name, body, env)
#
#     IO.puts "\n\n\n====== Actual Output ======\n"
#
#     if funs !== %{} do
#       funs
#       |> Enum.filter(&(!Map.has_key?(specs, elem(&1,0))))
#       |> Enum.map(fn {head, fun} -> "Fun missing specs: #{inspect head} -> #{inspect fun}" end)
#       |> Enum.join("\n")
#       |> IO.puts
#     end
#
#     # IO.puts "\n"
#     #
#     # check_fun_follows_spec_
#     # |> Enum.join("\n")
#     # |> IO.puts
#
#     IO.puts "\n"
#
#     # check_spec_fun_heads()
#
#     nil
#   end
#
#
#   def type_check_gather_types(_module_name, {:__block__, _attrs, expressions}, _env) do
#     for {:@, _, [{:type, _typeattrs, body_ast}]} <- expressions,
#         [{:::, _, [{name_ast, _, nil}, type_ast]}] = body_ast,
#         into: %{} do
#       {name_ast, type_ast}
#     end
#   end
#
#
#   def type_check_gather_specs(_module_name, {:__block__, _attrs, expressions}, _env) do
#     for {:@, _, [{:spec, _specattrs, body_ast}]} <- expressions,
#         [{:::, _, [{name_ast, _, func_def_ast}, func_ret_ast]}] = body_ast,
#         name_arity = length(func_def_ast),
#         into: %{} do
#       {{name_ast, name_arity}, {func_def_ast, func_ret_ast}}
#     end
#   end
#
#
#   def type_check_gather_funs(_module_name, {:__block__, _attrs, expressions}, _env) do
#     funs = for expression <- expressions,
#         {type, attrs, [fun_head_ast, fun_do_ast]} <- [expression],
#         type in [:def, :defp, :defmacro, :defmacrop],
#         name_ast = get_name_ast_from_head(fun_head_ast),
#         {name, _, args_ast} = name_ast,
#         name_arity = length(args_ast),
#         defaults = Enum.filter_map(args_ast, fn
#           {:'\\\\', _, _} -> true
#           _ -> false
#         end, fn {_, _, [arg_name, arg_default]} ->
#           arg_default
#         end),
#         count_defaults = length(defaults),
#         name_arity_min = name_arity - count_defaults
#         # into: %{},
#         do
#           if count_defaults === 0 do
#             {{name, name_arity}, expression}
#           else
#             [{{name, name_arity}, expression}
#             | build_fun_defaults(type, attrs, name, :lists.reverse(args_ast), [])
#               # for arity <- ((name_arity - 1) .. name_arity_min)
#               #   do
#               #     {{name, arity}, {type, attrs, []}
#               #     }
#               #   end
#             ]
#           end
#         end
#     funs |> List.flatten |> Enum.into(%{})
#   end
#
#
#   defp build_fun_defaults(type, attrs, name, head_ast_reversed, req_head)
#   defp build_fun_defaults(type, attrs, name, [], req_head), do: []
#   defp build_fun_defaults(type, attrs, name, [{:'\\\\', attr, default}|rest], req_head) do
#     [{{name, length(rest) + length(req_head)}, {type, attr, [:delegate_default]}}
#     | build_fun_defaults(type, attrs, name, rest, req_head)
#     ]
#   end
#   defp build_fun_defaults(type, attrs, name, [req|rest], req_head) do
#     build_fun_defaults(type, attrs, name, rest, [req|req_head])
#   end
#
#
#   def get_name_ast_from_head({:when, _, [name_ast|_]}), do: name_ast
#   def get_name_ast_from_head(name_ast), do: name_ast
#
#   def get_when_ast_from_head({:when, _, [_|when_ast]}), do: when_ast
#   def get_when_ast_from_head(_), do: []
#
#
#   def check_types(module_name, body, env)
#   def check_types(module_name, body, env) do
#     IO.inspect {:check_types, module_name, body, env}
#     # IO.inspect Keyword.keys(env.functions)
#     # env.functions |> Enum.map(&IO.inspect/1)
#     {[], []}
#   end
#
#
#   # Get type of:
#
#   # Should cache this... badly...
#   defp get_type_of() do
#   end
#
#   def __after_compile__(env, objbin) do
#     {:ok, {_modname, [abstract_code: abscode]}} = :beam_lib.chunks(objbin, [:abstract_code])
#     {:raw_abstract_v1, code} = abscode
#     code
#     |> Enum.reduce(%{opaques: [], types: [], specs: []}, fn
#       {:attribute, _line, :spec, {raw_fun, [raw_first_clause | _rest]} = newspec}, %{specs: spec} = acc ->
#         # IO.inspect {"Found spec", raw_fun, raw_first_clause, _rest}
#         %{acc | specs: [newspec|spec]}
#       {:attribute, _line, :type, {name, type_form, var_form} = newtype}, %{types: type} = acc ->
#         # IO.inspect {"Found type", name, type_form, var_form}
#         %{acc | types: [newtype|type]}
#       {:attribute, _line, :opaque, {name, type_form, var_form} = newopaque}, %{opaques: opaque} = acc ->
#         # IO.inspect {"Found opaque", name, type_form, var_form}
#         %{acc | opaques: [newopaque|opaque]}
#       _bc, acc ->
#         # IO.inspect {:blah, _bc}
#         acc
#     end)
#     |> IO.inspect
#   end
#
#   @spec get_module_types_funspecs(atom()) :: %{}
#   def get_module_types_funspecs(module) do
#     {^module, objbin, _filename} = :code.get_object_code(module)
#     {:ok, {^module, [abstract_code: abscode, exports: exports]}} = :beam_lib.chunks(objbin, [:abstract_code, :exports])
#     {:raw_abstract_v1, code} = abscode
#     code
#     |> Enum.reduce(%{opaques: [], types: [], specs: []}, fn
#       {:attribute, _line, :spec, {raw_fun, [raw_first_clause | _rest]} = newspec}, %{specs: spec} = acc ->
#         # IO.inspect {"Found spec", raw_fun, raw_first_clause, _rest}
#         %{acc | specs: [newspec|spec]}
#       {:attribute, _line, :type, {name, type_form, var_form} = newtype}, %{types: type} = acc ->
#         # IO.inspect {"Found type", name, type_form, var_form}
#         %{acc | types: [newtype|type]}
#       {:attribute, _line, :opaque, {name, type_form, var_form} = newopaque}, %{opaques: opaque} = acc ->
#         # IO.inspect {"Found opaque", name, type_form, var_form}
#         %{acc | opaques: [newopaque|opaque]}
#       _, acc -> acc
#     end)
#   end
#
#
# # def check_fun_follows_spec(module_name, {:__block__, _attrs, expressions}, _env) do
# #   do_check_fun_follows_spec(module_name, expressions)
# # end
# #
# # defp do_check_fun_follows_spec(module_name, expressions, spec \\ nil, funspec \\ nil)
# # defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | expressions], nil, nil) when fun_type in @fun_types do
# # IO.inspect "Blah1 #{inspect fun}"
# #   [msg(mn, fun, "Fun did not have a spec immediately before it") |
# #     do_check_fun_follows_spec(mn, expressions, nil, nil)]
# # end
# # defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | exprs], nil, funspec) when fun_type in @fun_types do
# # IO.inspect "Blah2 #{inspect fun}"
# #   case does_spec_match_fun_name(funspec, fun) do
# #     nil -> do_check_fun_follows_spec(mn, exprs, nil, funspec)
# #     err ->
# #       [msg(mn, fun, "Fun does not match the prior listed spec with error of \"#{err}\"", funspec) |
# #         do_check_fun_follows_spec(mn, exprs, nil, nil)]
# #   end
# # end
# # defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | _exprs] = exprs, spec, _f) when fun_type in @fun_types do
# # IO.inspect "Blah3 #{inspect fun}"
# #   do_check_fun_follows_spec(mn, exprs, nil, spec)
# # end
# # defp do_check_fun_follows_spec(mn, [{:@, _, [{:spec, _specattrs, _body_ast}]} = newspec | expressions], nil, _fs) do
# # IO.inspect "Blah4 #{inspect newspec}"
# #   do_check_fun_follows_spec(mn, expressions, newspec, nil)
# # end
# # defp do_check_fun_follows_spec(mn, [{:@, _, [{:spec, _specattrs, _body_ast}]} = newspec | expressions], spec, nil) do
# # IO.inspect "Blah5 #{inspect newspec}"
# #   [msg(mn, spec, "Spec found when prior spec already stated without function", newspec) |
# #     do_check_fun_follows_spec(mn, expressions, newspec, nil)]
# # end
# # defp do_check_fun_follows_spec(mn, [_e | expressions], nil, _fs) do
# # IO.inspect "Blah6 #{inspect _e}"
# #   do_check_fun_follows_spec(mn, expressions, nil, nil)
# # end
# # defp do_check_fun_follows_spec(mn, [_e | expressions], spec, _fs) do
# # IO.inspect "Blah7 #{inspect _e}"
# #   [msg(mn, spec, "Spec does not immediately preceed its function") |
# #     do_check_fun_follows_spec(mn, expressions, nil, nil)]
# # end
# # defp do_check_fun_follows_spec(_mn, [], nil, _fs) do
# # IO.inspect "Blah8"
# #   []
# # end
# # defp do_check_fun_follows_spec(mn, [], spec, _fs) do
# # IO.inspect "Blah9"
# #   [msg(mn, spec, "Spec does not immediately preceed any function")]
# # end
#
#
#   @opaque spec_ast :: nil
#   @opaque fun_ast :: nil
#
#   @spec does_spec_match_fun_name(spec_ast, fun_ast) :: boolean() | nil
#   defp does_spec_match_fun_name(spec, fun)
#   defp does_spec_match_fun_name(
#     {:@, _, [{:spec, spec_attrs, [{:::, _spec_attrs, [spec_head | spec_body]}]}]},
#     {fun_type, fun_attrs, [fun_head | fun_body]})
#   when fun_type in @fun_types do
#     {spec_name, _spec_name_attr, spec_name_args} = get_name_ast_from_head(spec_head)
#     {fun_name, _fun_name_attr, fun_name_args} = get_name_ast_from_head(fun_head)
# IO.puts "blorp\n#{inspect spec_name}\n#{inspect fun_name}"
#     if spec_name !== fun_name do
#       "Names do not match of spec #{inspect spec_name} and fun #{inspect fun_name}"
#     else
#       # TODO:  Test arity here
#       IO.puts "TODO: arity:\n#{inspect spec_name_args}\n#{inspect fun_name_args}"
#       nil
#     end
#   end
#   defp does_spec_match_fun_name(non_spec, non_fun) do
#     "ERROR:  Not spec or fun:\nNonSpec: #{inspect non_spec}\nNonFun: #{inspect non_fun}"
#   end
#
#
#   def msg(module_name, issue_obj, msg, rel_issue_obj \\ nil)
#   def msg(module_name, {_type, attr, _body} = issue_obj, msg, nil) do
#     "#{msg(module_name, attr, msg)}\t#{inspect issue_obj}\n"
#   end
#   def msg(module_name, {_type, attr, _body} = issue_obj, msg, rel_issue_obj) do
#     "#{msg(module_name, attr, msg)}\t#{inspect issue_obj}\n#{msg(module_name, rel_issue_obj, "Related")}"
#   end
#   def msg(module_name, [line: line], msg, _) do
#     "#{module_name}:#{to_string line}: #{msg}\n"
#   end
#   def msg(module_name, nil, msg, _) do
#     "#{module_name}: #{msg}\n"
#   end
#   def msg(module_name, issue_obj, msg, _) do
#     "#{module_name}: #{msg}\n\tIssue Object: #{inspect issue_obj}\n"
#  end

end
