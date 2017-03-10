defmodule MLElixir do
  @moduledoc """
  """



  alias TypedElixir.HMEnv
  alias TypedElixir.Type



  defmodule Key do
    def typename(name), do: {:typename, name}
    def func(name, arity), do: {:func, name, arity}
    def binding(name), do: {:binding, name}
    def generic(name), do: {:generic, name}
  end



  defmodule Core do

    def __ml_open__ do
      %{
        funs: %{
          # +: fn # Passes in the environment, meta of the call, and the calls arguments AST's
          #   (env, meta, [{_,leftMeta,_}, {_,rightMeta,_}]=args) ->
          #     left = leftMeta[:type]
          #     right = rightMeta[:type]
          #     {typeTag, type, _typeMeta} = MLElixir.unify_types!(env, left, right)
          #     # TODO:  Make addition refine the values properly on the type
          #     if typeTag === :"$$TCONST$$" and type in [:int, :float] do
          #       ast = {:"$$CALL$$", [type: {typeTag, type, []}] ++ meta, [{Kernel, :+} | args]}
          #       {env, ast}
          #     else
          #       raise %InvalidCall{message: "Invalid arguments types, only `int` or `float` is allowed", name: :+, meta: meta}
          #     end
          #   end,
        },
        types: %{
        },
      }
    end

  end



  defp debug(val, opts, section, prefix \\ nil)
  defp debug(val, %{user: opts}, section, prefix), do: debug(val, opts, section, prefix)
  defp debug(val, opts, section, prefix) do
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



  defmacro defmlmodule(name, opts) do #when is_atom(name) and is_list(opts) do
    block_module = opts[:do]
    opts = Keyword.delete(opts, :do)
    opts = Keyword.put(opts, :environment, __CALLER__)
    # opts = [debug: true] ++ opts

    env = %HMEnv{user: Enum.into(opts, %{})} |> HMEnv.push_scope(:top)
      |> debug(opts, :ENV)

    {env, name, name_meta} = parse_defmlmodule_name_meta(env, name)
      |> debug(env, :name)

    module_type = name_meta[:type]
      |> debug(env, :module_type_explicit)

    block_module = block_module
      |> debug(env, :module_block)
      |> block_if_not_block()

    {env, module_mlbody} = parse_defmlmodule_body(env, block_module)
      |> debug(env, :module_mlbody)

    # TODO:  Process module_mlbody to unique and sort the elements by name/arity, rename hidden things as well

    {env, inferred_module_type} = infer_module_type(env, module_mlbody)
      |> debug(env, :inferred_module_type)

    {env, module_type} = Type.resolve_types!(env, inferred_module_type, module_type)
      |> debug(env, :module_type)

    module_elixir_ast = convert_to_elixir_module_ast(env, name, name_meta, module_mlbody, module_type)
      |> debug(env, :module_elixir_ast)

    # {:defmodule, [context: __MODULE__, import: Kernel] ++ name_meta, [{:__aliases__, [alias: false], [name]}, [do: module_mlbody]]}
    #   |> debug(env, :module_ast)

    # throw "UNIMPLEMENTED: defmlmodule"
    module_elixir_ast
  end



  defp infer_module_type(env, module_mlbody) do
    {env, types} =
      Enum.reduce(module_mlbody, {env, %{}}, fn
        ({:type, meta, [name, _unresolved_type]}, {env, types}) ->
          {env, type} = Type.get_type_or_ptr_type(env, meta[:type])
          types = Map.put(types, name, type)
          {env, types}
        ({:def, _meta, [_name, _args, _body]}, {env, types}) ->
          # {env, type} = Type.get_type_or_ptr_type(env, meta[:type])
          # types = Map.put(types, name, type)
          # {env, types}
          {env, types}
        (unknown, {_env, types}) -> Process.sleep(500); throw {:TODO, :unhandled_module_type_builder, unknown, types}
      end)
    Type.Module.new(env, types)
  end



  defp parse_defmlmodule_name_meta(env, name)
  # Untyped module
  defp parse_defmlmodule_name_meta(env, name) when is_atom(name) do
    {env, type} = Type.Ptr.Unbound.new_ptr(env)
    {env, name, [type: type]}
  end
  defp parse_defmlmodule_name_meta(env, {:__aliases__, meta, [name]}) when is_atom(name) do
    {env, type} = Type.Ptr.Unbound.new_ptr(env)
    {env, name, [type: type] ++ meta}
  end
  # Typed module
  defp parse_defmlmodule_name_meta(env, {:|, _type_meta, [name_ast, type_ast]}) do
    {env, type} = parse_type_expression(env, type_ast)
    {env, name, meta} = parse_defmlmodule_name_meta(env, name_ast)
    {env, name, [type: type] ++ meta}
  end
  defp parse_defmlmodule_name_meta(_env, name) do
    throw {:TODO, :unhandled_module_name, name}
  end


  defp parse_defmlmodule_body(env, body)
  defp parse_defmlmodule_body(env, {:__block__, body_meta, body_exprs}) do
    {env, exprs} = HMEnv.map_env(env, body_exprs, &parse_module_expression/2)
    exprs = Enum.filter(exprs, &(&1))
    {env, exprs}
  end



  defp parse_module_expression(env, expr)
  # Module definition
  defp parse_module_expression(env, {:type, meta, [{:=, equals_meta, [{:__aliases__, _name_meta, [name]}, type_expr]}]}) when is_atom(name) do
    # TODO:  Ugh, wiped old code, let's do this better...
    throw {:TODO, :handle_module_definition_and_possible_refinement}
  end
  # Type definition
  defp parse_module_expression(env, {:type, meta, [{:=, equals_meta, [{name, _name_meta, name_context = nil}, type_expr]}]}) when is_atom(name) and is_atom(name_context) do
    # TODO:  Verify the type is not a module type
    {env, type} = parse_type_expression(env, type_expr)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # Type declaration
  defp parse_module_expression(env, {:type, meta, [{name, name_meta, name_context = nil}]}) when is_atom(name_context) do
    {env, type} = Type.Ptr.Generic.new_ptr(env, true, name: name)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # 'def' is basically 'let'
  defp parse_module_expression(env, {:def, def_meta, def_args}) do
    parse_module_def(env, def_meta, def_args)
  end
  # Also allow 'let' too
  defp parse_module_expression(env, {:let, def_meta, def_args}) do
    parse_module_def(env, def_meta, def_args)
  end
  defp parse_module_expression(_env, expr) do
    throw {:TODO, :unhandled_module_expression, expr}
  end



  defp parse_type_expression(env, type_expr)
  # Primitives
  defp parse_type_expression(env, {:integer, _meta, nil}), do: Type.Const.new(env, :integer)
  defp parse_type_expression(env, {:float, _meta, nil}), do: Type.Const.new(env, :float)
  # Literals
  defp parse_type_expression(env, integer) when is_integer(integer), do: Type.Const.new(env, :integer, values: [integer])
  defp parse_type_expression(env, float) when is_float(float), do: Type.Const.new(env, :float, values: [float])
  # Named Generic
  defp parse_type_expression(env, {:+, _meta, [{name, _name_meta, name_context = nil}]}) when is_atom(name) and is_atom(name_context) do
    key = Key.generic(name)
    case HMEnv.get_type(env, key) do
      nil ->
        {env, type} = Type.Ptr.Generic.new_ptr(env, true, name: name)
        env = HMEnv.push_type(env, key, type)
        {env, type}
      type -> {env, type}
    end
  end
  # Type refinement
  defp parse_type_expression(env, {{:., _dot_meta, type_name_ast}, _meta, refinements}) do
    unrefined_type =
      case type_name_ast do
        # Module type itself
        [{:__aliases__, _module_name_meta, names}] when is_list(names) ->
          module = Module.concat(names)
          {:module, ^module} = wait_ensure_loaded(module)
          ml_module_info = module.ml_module_info()
          ml_module_info.type # The module_type
        # Type in module
        [{:__aliases__, _module_name_meta, names}, type_name] when is_list(names) and is_atom(type_name) ->
          module = Module.concat(names)
          Process.sleep(500)
          {:module, ^module} = wait_ensure_loaded(module)
          ml_module_info = module.ml_module_info()
          case ml_module_info.type.types[type_name] do
            nil -> throw {:NO_TYPE_ON_MODULE, module, type_name}
            type -> type
          end
          # throw {:TODO, :get_type_from_module_type, ml_module_info}
        unknown -> throw {:TODO, :unhandled_module_refinement_module_name, unknown}
      end
    case {refinements, unrefined_type} do
      {[], %Type.Const{} = type} -> {env, type} # Cannot refine a Type.Const, nothing 'to' refine...
      {[], %Type.Module{} = type} -> {env, type}
      {[refinements], %Type.Module{} = type} when is_list(refinements) ->
        {env, type} =
          Enum.reduce(refinements, {env, type}, fn
            ({k, new_type_ast}, {env, type}) ->
              case type.types[k] do
                nil -> throw {:invalid_typename_access, k, :does_not_exist_on, type}
                old_subtype ->
                  {env, new_type} = parse_type_expression(env, new_type_ast)
                  {env, new_type} = Type.resolve_types!(env, new_type, old_subtype)
                  type = %{type | types: %{type.types | k => new_type}}
                  {env, type}
              end
          end)
      unknown -> throw {:TODO, :unhandled_exported_type, unknown}
    end
  end
  # Named type
  defp parse_type_expression(env, {name, _meta, nil}) when is_atom(name) do
    key = Key.typename(name)
    case HMEnv.get_type(env, key) do
      nil -> throw {:TYPENAME_DOES_NOT_EXIST, name}
      type -> {env, type}
    end
  end
  defp parse_type_expression(_env, type_expr) do
    throw {:TODO, :unhandled_type_expression, type_expr}
  end


  defp wait_ensure_loaded(module, times \\ 20, sleep \\ 50)
  defp wait_ensure_loaded(module, times, _sleep) when times <= 0 do
    Code.ensure_compiled(module)
  end
  defp wait_ensure_loaded(module, times, sleep) do
    # TODO:  Figure out how to tell elixir to stop until this thing is loaded...
    case Code.ensure_compiled(module) do
      {:module, ^module} -> {:module, module}
      _ ->
        Process.sleep(sleep)
        wait_ensure_loaded(module, times - 1, sleep)
    end
  end



  defp parse_module_def(env, meta, definition)
  # With type and body 0-arg
  # defp parse_module_def(env, meta, [{:|, _split_meta, [{name, _name_meta, name_context = nil}, {:=, _equals_meta, [type_expr, body_expr]}]}]) when is_atom(name) and is_atom(name_context) do
  #   env = HMEnv.push_scope(env, :def, name)
  #   {env, type} = parse_type_expression(env, type_expr)
  #   # key = Key.typename(name)
  #   # env = HMEnv.push_type(env, key, type)
  #   {env, body} = parse_ml_expression(env, body_expr)
  #   {_, body_meta, _} = body
  #   body_type = body_meta[:type]
  #   # TODO:  Make sure the body_type fulfills the type
  #   Type.resolve_types!(env, type, body_type)
  #   {env, _scope} = HMEnv.pop_scope(env, :def)
  #   # key = Key.func(name)
  #   # env = HMEnv.push_type(env, key, type)
  #   ast = {:def, [type: type] ++ meta, [name, [], body]}
  #   {env, ast}
  # end
  # With type and body
  defp parse_module_def(env, meta, [{:|, _split_meta, [{name, _name_meta, args_ast}, {:=, _equals_meta, [type_expr, body_expr]}]}]) when is_atom(name) do
    {env, return_type} = parse_type_expression(env, type_expr)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
    # env = HMEnv.push_scope(env, :def, name)
    # {env, args} = parse_module_def_args(env, args_ast)
    # args_types = Enum.map(args, fn {_, meta, _} -> meta[:type] end)
    # {env, return_type} = parse_type_expression(env, type_expr)
    # {env, inner_type} = Type.Func.new(env, args_types, return_type, false)
    # func_key = Key.func(name, length(args))
    # env = HMEnv.push_type(env, func_key, inner_type)
    # {env, body} = parse_ml_expression(env, body_expr)
    # {_, body_meta, _} = body
    # body_type = body_meta[:type]
    # {env, return_type} = Type.resolve_types!(env, body_type, return_type)
    # {env, _scope} = HMEnv.pop_scope(env, :def)
    # {env, func_type} = Type.Func.new(env, args_types, return_type, false)
    # env = HMEnv.push_type(env, func_key, func_type)
    # ast = {:def, [type: func_type] ++ meta, [name, args, body]}
    # {env, ast}
  end
  defp parse_module_def(env, meta, [{:|, _split_meta, [{name, _name_meta, args_ast}, type_expr]}, [do: body_expr]]) when is_atom(name) do
    {env, return_type} = parse_type_expression(env, type_expr)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
  end
  defp parse_module_def(_env, _meta, [{:|, _, _}|_]=definition) do
    throw {:TODO, :unhandled_typed_def_expression, definition}
  end
  # # No type with body 0-arg
  # defp parse_module_def(env, meta, [{:=, _equals_meta, [{name, _name_meta, name_context = nil}, body_expr]}]) when is_atom(name) and is_atom(name_context) do
  #   {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
  #   parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
  #   # env = HMEnv.push_scope(env, :def, name)
  #   # {env, body} = parse_ml_expression(env, body_expr)
  #   # {_, body_meta, _} = body
  #   # type = body_meta[:type]
  #   # {env, _scope} = HMEnv.pop_scope(env, :def)
  #   # ast = {:def, [type: type] ++ meta, [name, [], body]}
  #   # {env, ast}
  # end
  # No type with body
  defp parse_module_def(env, meta, [{:=, _equals_meta, [{name, _name_meta, args_ast}, body_expr]}]) when is_atom(name) do
    {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
    # env = HMEnv.push_scope(env, :def, name)
    # {env, args} = parse_module_def_args(env, args_ast)
    # args_types = Enum.map(args, fn {_, meta, _} -> meta[:type] end)
    # {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
    # {env, inner_type} = Type.Func.new(env, args_types, return_type, false)
    # func_key = Key.func(name, length(args))
    # env = HMEnv.push_type(env, func_key, inner_type)
    # {env, body} = parse_ml_expression(env, body_expr)
    # {_, body_meta, _} = body
    # body_type = body_meta[:type]
    # {env, return_type} = Type.resolve_types!(env, body_type, return_type)
    # {env, _scope} = HMEnv.pop_scope(env, :def)
    # {env, func_type} = Type.Func.new(env, args_types, return_type, false)
    # env = HMEnv.push_type(env, func_key, func_type)
    # ast = {:def, [type: func_type] ++ meta, [name, args, body]}
    # {env, ast}
  end
  # No type with block body
  defp parse_module_def(env, meta, [{name, _name_meta, args_ast}, [do: body_expr]]) when is_atom(name) do
    {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
  end
  defp parse_module_def(_env, _meta, definition) do
    throw {:TODO, :unhandled_def_expression, definition}
  end


  defp parse_module_def_impl(env, meta, name, args, return_type, body_expr)
  defp parse_module_def_impl(env, meta, name, nil, return_type, body_expr), do: parse_module_def_impl(env, meta, name, [], return_type, body_expr)
  defp parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr) when is_atom(name) and is_list(args_ast) do
    env = HMEnv.push_scope(env, :def, name)
    {env, args} = parse_module_def_args(env, args_ast)
    args_types = Enum.map(args, fn {_, meta, _} -> meta[:type] end)
    {env, inner_type} = Type.Func.new(env, args_types, return_type, false)
    func_key = Key.func(name, length(args))
    env = HMEnv.push_type(env, func_key, inner_type)
    {env, body} = parse_ml_expression(env, body_expr)
    {_, body_meta, _} = body
    body_type = body_meta[:type]
    {env, return_type} = Type.resolve_types!(env, body_type, return_type)
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {env, func_type} = Type.Func.new(env, args_types, return_type, false)
    env = HMEnv.push_type(env, func_key, func_type)
    ast = {:def, [type: func_type] ++ meta, [name, args, body]}
    {env, ast}
  end



  # defp get_empty_fun_type(env, name, args_types, return_type \\ nil) do
  #   {env, return_type} =
  #     case return_type do
  #       nil -> Type.Ptr.Unbound.new_ptr(env)
  #       type -> {env, type}
  #     end
  #   {env, type} = Type.Func.new(env, args_types, return_type, false)
  # end



  defp parse_module_def_args(env, args)
  defp parse_module_def_args(env, args) do
    {env, arg_asts} = HMEnv.map_env(env, args, &parse_module_def_head_arg/2)
    {env, arg_asts}
  end


  defp parse_module_def_head_arg(env, arg)
  # Typed Binding
  defp parse_module_def_head_arg(env, {:|, _meta, [{name, name_meta, context = nil}, type_ast]}) when is_atom(name) and is_atom(context) do
    # {env, {arg_first, arg_meta, arg_third} = arg} = parse_module_def_head_arg(env, arg_ast)
    # arg_type = arg_meta[:type]
    {env, type} = parse_type_expression(env, type_ast)
    # {env, type} = Type.resolve_types!(env, type, arg_type)
    # if type === arg_type do
    #   {env, arg}
    # else
    #   key = Key.binding(name)
    #   env = HMEnv.push_type(env, key, type)
    #   arg = {arg_first, [type: type] ++ arg_meta, arg_third}
    #   {env, arg}
    # end
    key = Key.binding(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:binding, [type: type] ++ name_meta, name}
    {env, ast}
  end
  # Untyped Binding
  defp parse_module_def_head_arg(env, {name, meta, context = nil}) when is_atom(name) and is_atom(context) do
    {env, type} = Type.Ptr.Generic.new_ptr(env, false)
    key = Key.binding(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:binding, [type: type] ++ meta, name}
    {env, ast}
  end
  defp parse_module_def_head_arg(env, arg) do
    throw {:TODO, :unhandled_def_head_arg, arg}
  end



  defp parse_ml_expression(env, expr)
  # Binding
  defp parse_ml_expression(env, {name, meta, context = nil}) when is_atom(name) and is_atom(context) do
    key = Key.binding(name)
    case HMEnv.get_type(env, key) do
      nil -> throw {:BINDING_NOT_FOUND, name}
      type ->
        ast = {:binding, [type: type] ++ meta, [name]}
        {env, ast}
    end
  end
  # Literals
  defp parse_ml_expression(env, integer) when is_integer(integer) do
    {env, type} = parse_type_expression(env, integer)
    {env, {:const, [type: type], integer}}
  end
  defp parse_ml_expression(env, float) when is_float(float) do
    {env, type} = parse_type_expression(env, float)
    {env, {:const, [type: type], float}}
  end
  defp parse_ml_expression(_env, expr) do
    throw {:TODO, :unhandled_ml_expression, expr}
  end



  defp block_if_not_block(ast)
  defp block_if_not_block({:__block__, meta, exprs}=ast) when is_list(meta) and is_list(exprs), do: ast
  defp block_if_not_block(ast), do: {:__block__, [], [ast]}




  defp convert_to_elixir_module_ast(env, nam, name_metae, module_mlast, module_type)
  defp convert_to_elixir_module_ast(env, name, name_meta, module_mlasts, module_type) when is_atom(name) and is_list(module_mlasts) do
    name_ast = {:__aliases__, [alias: false], [name]}
    body_ast = Enum.map(module_mlasts, &convert_to_elixir_module_body_ast(env, &1)) |> Enum.filter(&(&1))
    ml_module_info_ast = generate_ml_module_info(env, module_type)
    modulebody_ast = [name_ast, [do: {:__block__, [], body_ast ++ ml_module_info_ast}]]
    defmodule_ast = {:defmodule, [context: __MODULE__, import: Kernel] ++ name_meta, modulebody_ast}
    # throw env.types
  end
  defp convert_to_elixir_module_ast(env, name, name_meta, module_mlast, module_type) do
    throw [:TODO, :convert_to_elixir_module_ast, name, name_meta, module_mlast]
  end



  defp generate_ml_module_info(env, module_type) do
    ml_module_info_ast = quote do
      def ml_module_info do
        %{
          type: unquote(Macro.escape(module_type))
        }
      end
    end
    [ml_module_info_ast]
  end



  defp convert_to_elixir_module_body_ast(env, module_expr)
  defp convert_to_elixir_module_body_ast(env, {:type, _, _}), do: nil # Ignore type definitions for per-expression code ast
  defp convert_to_elixir_module_body_ast(env, {:def, def_meta, [name, args, body_expr]}) when is_atom(name) and is_list(args) and not is_list(body_expr) do
    args = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    name_ast = {name, [], args}
    body_ast = convert_to_elixir_module_body_ast(env, body_expr)
    ast = {:def, def_meta, [name_ast, [do: body_ast]]}
    ast
  end
  defp convert_to_elixir_module_body_ast(env, {:binding, meta, [name]}) when is_atom(name) do
    {name, meta, nil}
  end
  defp convert_to_elixir_module_body_ast(env, {:binding, meta, name}) when is_atom(name) do
    {name, meta, nil}
  end
  defp convert_to_elixir_module_body_ast(env, {:const, _const_meta, value}), do: value
  defp convert_to_elixir_module_body_ast(_env, module_expr) do
    throw {:TODO, :unhandled_module_body_expr, module_expr}
  end


  # defmacro defml(opts) when is_list(opts) do
  #   case opts[:do] do
  #     nil ->
  #       [ast] = opts
  #       defml_impl(ast, [])
  #     ast when is_tuple(ast) -> defml_impl(ast, opts)
  #   end
  # end
  # defmacro defml(expr) do
  #   defml_impl(expr, [])
  # end
  #
  # defp defml_impl(expr, opts) do
  #   # IO.inspect {:ML, expr}
  #   no_default_opens = opts[:no_default_opens] || false
  #   env = if no_default_opens, do: %MLEnv{}, else: open_module(%MLEnv{}, MLElixir.Core)
  #   env = case opts[:open] do
  #     nil -> env
  #     module when is_atom(module) ->
  #       open_module(env, module)
  #     modules when is_list(modules) ->
  #       Enum.reduce(modules, env, fn(module, env) -> open_module(env, module) end)
  #   end
  #   {ml_env, ml_ast} = parse_ml_expr(env, expr)
  #   # IO.inspect {:MLAST, ml_ast}
  #   # IO.inspect {:MLENV, ml_env}
  #   ast = reify_ml_expr(ml_env, ml_ast)
  #   # IO.inspect {:MLDONE, ast}
  #   ast
  # end

end
