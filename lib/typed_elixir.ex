defmodule TypedElixir do
  @moduledoc """
  """


  defmodule HMEnv do
    defstruct counter: -1, types: %{}, scopes: [], type_vars: %{}


    def new_counter(env) do
      counter = env.counter+1
      env = %{env | counter: counter}
      {env, counter}
    end


    # Type helpers

    def push_type(env, key, type) do
      types = Map.update(env.types, key, [type], fn prior -> [type | prior] end)
      env = %{env | types: types}
      env = add_to_scope(env, :type, key)
      env
    end

    def get_type(env, key) do
      case env.types[key] do
        nil -> nil
        [] -> nil
        [type | _] -> type
      end
    end

    defp pop_type(env, key) do
      types = case env.types[key] do
        nil -> throw {:POP_TYPE, key, "does not exist"}
        [] -> throw {:POP_TYPE, key, "does not exist"}
        [_] -> Map.delete(env.types, key)
        [_ | rest] -> Map.put(env.types, key, rest)
      end
      %{env | types: types}
    end


    # Type vars

    def push_type_var(env, id, type) do
      type_vars = Map.update(env.type_vars, id, [type], fn prior -> [type | prior] end)
      env = %{env | type_vars: type_vars}
      # env = add_to_scope(env, :type_var, id) # Unique ID for an entire module, plus might be used elsewhere...
      env
    end

    def get_type_var(env, id) do
      case env.type_vars[id] do
        nil -> throw {:GET_TYPE_VAR, id, "does not exist", env.type_vars}
        [] -> throw {:GET_TYPE_VAR, id, "does not exist", env.type_vars}
        [type_vars | _] -> type_vars
      end
    end

    # Never popping type vars because the types may be referenced elsewhere
    # defp pop_type_var(env, id) do
    #   type_vars = case env.type_vars[id] do
    #     nil -> throw {:POP_TYPE_VAR, id, "does not exist"}
    #     [] -> throw {:POP_TYPE_VAR, id, "does not exist"}
    #     [_] -> Map.delete(env.type_vars, id)
    #     [_ | rest] -> Map.put(env.type_vars, id, rest)
    #   end
    #   %{env | type_vars: type_vars}
    # end

    def update_type_var(env, id, type) do
      type_vars = Map.update!(env.type_vars, id, fn [_ | prior] -> [type | prior] end)
      env = %{env | type_vars: type_vars}
      env
    end


    # Scope helpers

    def push_scope(env, id, doc \\ nil) do
      scopes = [{id, doc, []} | env.scopes]
      %{env | scopes: scopes}
    end

    def pop_scope(env, id) do
      [{^id, _, scope} | scopes] = env.scopes
      env = %{env | scopes: scopes}
      env = Enum.reduce(scope, env, fn ({key, value}, env) -> removed_from_scope(env, key, value) end)
      {env, scope}
    end

    def get_scope(env) do
      [scope | scopes] = env.scopes
      scope
    end

    defp add_to_scope(env, key, val) do
      [{id, doc, scope} | restScopes] = env.scopes
      scopes = [{id, doc, [{key, val} | scope]} | restScopes]
      %{env | scopes: scopes}
    end

    defp removed_from_scope(env, key, val)
    defp removed_from_scope(env, :type, name), do: pop_type(env, name)
    # defp removed_from_scope(env, :type_var, id), do: pop_type_var(env, id)
  end



  defmodule TypeGeneric       ,do: defstruct []
  defmodule TypeConst         ,do: defstruct type: :"$$NO$TYPE$$", meta: %{}
  defmodule TypeCall          ,do: defstruct args: nil, return: %TypeGeneric{} # blah(args)
  defmodule TypeCallIndirect  ,do: defstruct args: nil, return: %TypeGeneric{} # blah.(args)
  defmodule TypeModule        ,do: defstruct []
  defmodule TypePtr do
    defstruct ptr: -1
    def new(env, subtype \\ %TypeGeneric{}) do
      {env, ptr} = HMEnv.new_counter(env)
      type = %TypePtr{ptr: ptr}
      env = HMEnv.push_type_var(env, ptr, subtype)
      {env, type}
    end
  end

  defmodule Type do
    # Refined containers
    defmodule Plain   ,do: defstruct [:type]
    defmodule Named   ,do: defstruct [:name, :type]
    defmodule Refined ,do: defstruct [:name, :type, :meta]

    # Types
    defmodule Const   ,do: defstruct [:name]
    defmodule App     ,do: defstruct [:name]
    defmodule Func    ,do: defstruct [:args_rtype, :return_rtype]
    defmodule Var     ,do: defstruct [:var_ptr]

    # Var Types
    defmodule Unbound ,do: defstruct [:id, :level]
    defmodule Link    ,do: defstruct [:type]
    defmodule Generic ,do: defstruct [:id]
  end




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
    env = HMEnv.push_scope(env, :module, name)
    {env, bodies} = Enum.reduce(body_asts, {env, []}, fn(module_entry_ast, {env, bodies}) ->
      {env, body} = type_check_body(env, opts, module_entry_ast)
      {env, [body | bodies]}
    end)
    bodies = :lists.reverse(bodies)
    Enum.map(bodies, &debug(&1, opts, :MODULE_BODY, name))
    # TODO:  Parse out the module information to make a type-map in a callback function
    {env, _scope} = HMEnv.pop_scope(env, :module)
    type = %TypeModule{}
    ast = {:__block__, [type: type] ++ meta, bodies}
    {env, ast}
  end



  defp type_check_body(env, opts, module_entry_ast)
  defp type_check_body(env, opts, {:def, meta, [head_ast, body_ast]}) do
    {env, head} = type_check_def_head(env, opts, head_ast) # This will push scope with the name
    {env, body} = type_check_def_body(env, opts, body_ast)
    func_type = get_type_of(head)
    return_type = get_type_of(body)
    # {env, return_type} = unify_types!(env, func_type.return, return_type)
    resolve_types!(env, return_type, func_type.return) # Can the return_type be returned by the func returns?
    func_type = %{func_type | return: return_type}
    ast = {:def, [type: func_type] ++ meta, [head, body]}
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {env, ast}
  end
  defp type_check_body(env, opts, {:@, attrMeta, [attr_ast]}) do
    {env, attr} = type_check_attribute(env, opts, attr_ast)
    ast = {:@, attrMeta, [attr]}
    {env, ast}
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
  # Direct function call
  defp type_check_expression(env, opts, {name, meta, args_ast}) when is_atom(name) and is_list(args_ast) do
    arity = length(args_ast)
    key = {:function, name, arity}
    {env, args} = Enum.reduce(args_ast, {env, []}, fn(arg_ast, {env, args}) ->
      {env, arg} = type_check_expression(env, opts, arg_ast)
      {env, args ++ [arg]}
    end)
    args_types = Enum.map(args, &get_type_of/1)
    case HMEnv.get_type(env, key) do
      nil -> throw {:FUNCTION_NOT_FOUND, name, arity, env.types}
      %TypeCall{args: call_args}=type when length(call_args)===arity ->
        _ = Enum.zip(call_args, args_types) |> Enum.map(fn({fromType, intoType}) ->
          resolve_types!(env, fromType, intoType)
        end)
        ast = {name, [type: type.return] ++ meta, args}
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
    env = HMEnv.push_scope(env, :def, name)
    argCount = length(args_ast)
    case HMEnv.get_type(env, {:function, name, argCount}) do
      nil -> # No pre-defined type for this fun-name, use the inferred value
        {env, args} = type_check_fun_bindings(env, opts, args_ast)
        {env, args} = type_check_fun_when(env, opts, args, when_ast)
        args_types = Enum.map(args, &get_type_of/1)
        type = %TypeCall{args: args_types}
        ast = {name, [type: type] ++ meta, args}
        {env, ast}
      %TypeCall{args: tArgs} = callType when length(tArgs)===argCount ->
        {env, args} = type_check_fun_bindings(env, opts, args_ast, tArgs)
        {env, args} = type_check_fun_when(env, opts, args, when_ast)
        args_types = Enum.map(args, &get_type_of/1)
        _ = Enum.zip(callType.args, args_types) |> Enum.map(fn({fromType, intoType}) ->
          resolve_types!(env, fromType, intoType)
        end)
        ast = {name, [type: callType] ++ meta, args}
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
      {env, arg} = type_check_binding(env, opts, arg_ast)
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



  defp type_check_binding(env, opts, binding, type \\ nil)
  defp type_check_binding(env, _opts, {name, meta, scope}, type) when is_atom(name) and is_atom(scope) do
    {env, type} = case type do
      nil ->
        TypePtr.new(env)
      type ->
        TypePtr.new(env, type)
        # {env, type}
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
    {env, type} = TypePtr.new(env, %TypeGeneric{})
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
    {env, argsTypes} = Enum.reduce(args_ast, {env, []}, fn (arg_ast, {env, args}) ->
      # {env, arg} = parse_type_from_specattr_arg(env, opts, arg_ast)
      {env, arg} = parse_type_from(env, opts, arg_ast)
      {env, [arg | args]}
    end)
    argsTypes = :lists.reverse(argsTypes)
    {env, returnType} = parse_type_from(env, opts, return_ast)
    type = %TypeCall{args: argsTypes, return: returnType}
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
  defp parse_type_from(env, _opts, atom) when is_atom(atom), do: {env, %TypeConst{type: :atom, meta: %{values: [atom]}}}
  defp parse_type_from(env, _opts, int) when is_integer(int), do: {env, %TypeConst{type: :integer, meta: %{values: [int]}}}
  defp parse_type_from(env, _opts, float) when is_float(float), do: {env, %TypeConst{type: :float, meta: %{values: [float]}}}
  # 0-arg built-in types
  defp parse_type_from(env, _opts, {built_in, _meta, []}) when built_in in [:integer, :float], do: {env, %TypeConst{type: built_in}}
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
  defp get_type_of(thing)
  defp get_type_of(atom) when is_atom(atom), do: %TypeConst{type: :atom, meta: %{values: [atom]}}
  defp get_type_of(int) when is_integer(int), do: %TypeConst{type: :integer, meta: %{values: [int]}}
  defp get_type_of(float) when is_float(float), do: %TypeConst{type: :float, meta: %{values: [float]}}
  defp get_type_of([do: do_ast]), do: get_type_of(do_ast)
  defp get_type_of({_, meta, _}), do: Keyword.fetch!(meta, :type)






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


  def resolve_types(env, fromType, intoType)
  def resolve_types(_env, type, type), do: type
  def resolve_types(env, type, %TypeGeneric{}), do: {env, type} |> IO.inspect
  # Keep TypePtr handling last
  def resolve_types(env, %TypePtr{ptr: fromPtr}=from, %TypePtr{ptr: intoPtr}=into) do
    intoType = HMEnv.get_type_var(env, intoPtr)
    intoPath = resolve_types(env, from, intoType)
    if Exception.exception?(intoPath) do
      fromType = HMEnv.get_type_var(env, fromPtr)
      resolve_types(env, fromType, into)
    else
      intoPath
    end
  end
  def resolve_types(env, fromType, %TypePtr{ptr: ptr}) do
    intoType = HMEnv.get_type_var(env, ptr)
    resolve_types(env, fromType, intoType)
  end
  def resolve_types(env, %TypePtr{ptr: ptr}, intoType) do
    fromType = HMEnv.get_type_var(env, ptr)
    resolve_types(env, fromType, intoType)
  end
  # Catch-all
  def resolve_types(_env, fromType, intoType) do
    throw {:NO_TYPE_RESOLUTION, fromType, intoType}
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


  def unify_types(env, t0, t1)
  def unify_types(env, t, t), do: {env, t}
  def unify_types(env, %TypeGeneric{}, t), do: {env, t}
  def unify_types(env, t, %TypeGeneric{}), do: {env, t}
  # Keep TypePtr handling last
  def unify_types(env, %TypePtr{ptr: ptr0}=t0, %TypePtr{ptr: ptr1}=t1) do
    type0 = HMEnv.get_type_var(env, ptr1)
    path0 = unify_types(env, t0, type0)
    if Exception.exception?(path0) do
      type1 = HMEnv.get_type_var(env, ptr0)
      unify_types(env, type1, t1)
    else
      path0
    end
  end
  def unify_types(env, t0, %TypePtr{ptr: ptr}) do
    t1 = HMEnv.get_type_var(env, ptr)
    unify_types(env, t0, t1)
  end
  def unify_types(env, %TypePtr{ptr: ptr}, t1) do
    t0 = HMEnv.get_type_var(env, ptr)
    unify_types(env, t0, t1)
  end
  # Catch-all
  def unify_types(_env, t0, t1) do
    throw {:NO_TYPE_UNIFICATION, t0, t1}
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
