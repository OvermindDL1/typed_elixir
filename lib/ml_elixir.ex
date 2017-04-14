defmodule MLElixir do
  @moduledoc """
  """


  # TODO:  BIG_TODO!  Refactor the error reporting from random throw's to actual exceptions that hold the ast meta information and such...


  alias TypedElixir.HMEnv
  alias TypedElixir.Type



  defmodule Key do
    def typename(name), do: {:typename, name}
    def func(name, arity), do: {:func, name, arity}
    def binding(name), do: {:binding, name}
    def generic(name), do: {:generic, name}
    def enumeration(name), do: {:enumeration, name}
    def call(name), do: {:call, name}
  end


  defp add_call(env, name, call_type, type) do
    key = Key.call(name)
    value = {call_type, type}
    HMEnv.push_type(env, key, value)
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



  defmacro defmlmodule(name, opts) do
    block_module = opts[:do]
    opts = Keyword.delete(opts, :do)
    opts = Keyword.put(opts, :environment, __CALLER__)
opts = Keyword.put(opts, :full_stack, true)
    case opts[:full_stack] do
      true -> defmlmodule_impl(name, block_module, opts)
      _ ->
        try do
          defmlmodule_impl(name, block_module, opts)
        catch
          x -> quote(do: throw unquote(x))
        end
    end
  end



  defp defmlmodule_impl(name, block_module, opts) do #when is_atom(name) and is_list(opts) do
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
        ({:external, _meta, _args}, {env, types}) ->
          {env, types}
        (unknown, {_env, types}) -> throw {:TODO, :unhandled_module_type_builder, unknown, types}
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
  defp parse_defmlmodule_body(env, {:__block__, _body_meta, body_exprs}) do
    {env, exprs} = HMEnv.map_env(env, body_exprs, &parse_module_expression/2)
    exprs = Enum.filter(exprs, &(&1))
    {env, exprs}
  end



  defp parse_module_expression(env, expr)
  # Module definition
  defp parse_module_expression(env, {:type, _meta, [{:=, _equals_meta, [{:__aliases__, _name_meta, [name]}, type_expr]}]}) when is_atom(name) do
    {env, type} = parse_type_expression(env, type_expr)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # Type definition
  defp parse_module_expression(env, {:type, _meta, [{:=, _equals_meta, [{name, _name_meta, args}, type_expr]}]}) when args === [] or args === nil do
    # TODO:  Verify the type is not a module type
    {env, type} = parse_type_expression(env, type_expr)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # Type definition with args
  defp parse_module_expression(env, {:type, _meta, [{:=, _equals_meta, [{name, _name_meta, args}, type_expr]}]}) when is_list(args) do
    env = HMEnv.push_scope(env, :type, name)
    {env, args_types} = parse_type_bindings(env, args)
    {env, type} = parse_type_expression(env, type_expr)
    {env, _scope} = HMEnv.pop_scope(env, :type)
    {env, type} = Type.App.new(env, type, args_types)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # Type enum definition
  defp parse_module_expression(env, {:type, meta, [{:|, _bar_meta, args}]}) do
    parse_module_type_enum(env, meta, args)
  end
  # Type declaration
  defp parse_module_expression(env, {:type, _meta, [{name, _name_meta, name_context = nil}]}) when is_atom(name_context) do
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
  # Also allow 'let' as well
  defp parse_module_expression(env, {:let, def_meta, def_args}) do
    parse_module_def(env, def_meta, def_args)
  end
  # 'external' makes a function that calls out to untyped erlang/elixir/whatever
  defp parse_module_expression(env, {:external, ext_meta, ext_args}) do
    parse_module_external(env, ext_meta, ext_args)
  end
  defp parse_module_expression(env, {:defexternal, ext_meta, ext_args}) do
    parse_module_external(env, ext_meta, ext_args)
  end
  defp parse_module_expression(_env, expr) do
    throw {:TODO, :unhandled_module_expression, expr}
  end




  defp parse_type_bindings(env, args) do
    HMEnv.map_env(env, args, fn
      (env, {name, _name_meta, nil}) ->
        {env, type} = Type.Ptr.Unbound.new_ptr(env)
        key = Key.typename(name)
        env = HMEnv.push_type(env, key, type)
        {env, {name, type}}
      (_env, arg) -> throw {:TODO, :parse_type_bindings, arg}
    end)
  end

  defp parse_type_expression(env, type_expr)
  # Primitives
  defp parse_type_expression(env, {:integer, _meta, nil}), do: Type.Const.new(env, :integer)
  defp parse_type_expression(env, {:float, _meta, nil}), do: Type.Const.new(env, :float)
  # Literals
  defp parse_type_expression(env, integer) when is_integer(integer), do: Type.Const.new(env, :integer, values: [integer])
  defp parse_type_expression(env, float) when is_float(float), do: Type.Const.new(env, :float, values: [float])
  # Named Generic
  defp parse_type_expression(env, {:!, _meta, [{name, _name_meta, name_context = nil}]}) when is_atom(name) and is_atom(name_context) do
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
  defp parse_type_expression(env, {{:., dot_meta, type_name_ast}, _meta, refinements}) do
    unrefined_type =
      case type_name_ast do
        # Module Name or type itself
        [{:__aliases__, _module_name_meta, [name]}] when is_atom(name) ->
          # Test if it is a typename first
          key = Key.typename(name)
          case HMEnv.get_type(env, key) do
            nil ->
              module = Module.concat([name])
              {:module, ^module} = wait_ensure_loaded(module)
              ml_module_info = module.ml_module_info()
              ml_module_info.type # The module_type
            type -> {env, type}
          end
        # Module type itself
        [{:__aliases__, _module_name_meta, names}] when is_list(names) ->
          module = Module.concat(names)
          {:module, ^module} = wait_ensure_loaded(module)
          ml_module_info = module.ml_module_info()
          ml_module_info.type # The module_type
        # Type in module type name or the module itself
        [{:__aliases__, _module_name_meta, [name]}, type_name] when is_atom(name) and is_atom(type_name) ->
          # Test if it is a typename first
          key = Key.typename(name)
          case HMEnv.get_type(env, key) do
            nil ->
              module = Module.concat([name])
              {:module, ^module} = wait_ensure_loaded(module)
              ml_module_info = module.ml_module_info()
              case ml_module_info.type.types[type_name] do
                nil -> throw {:NO_TYPE_ON_MODULE, dot_meta[:line], module, type_name}
                type -> type
              end
            %Type.Module{types: types} ->
              case types[type_name] do
                nil -> throw {:NO_TYPE_ON_MODULE_TYPE, dot_meta[:line], name, type_name}
                type -> type
              end
            type -> throw {:UNSUPPORTED_TYPE, dot_meta[:line], name, type_name, type}
          end
        # Type in module
        [{:__aliases__, _module_name_meta, names}, type_name] when is_list(names) and is_atom(type_name) ->
          module = Module.concat(names)
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
        {env, type}
      unknown -> throw {:TODO, :unhandled_exported_type, unknown}
    end
  end
  # 2-tuple Record type, fix to normal call because Elixir AST design...
  defp parse_type_expression(env, {arg0, arg1}), do: parse_type_expression(env, {:{}, [], [arg0, arg1]})
  # defp parse_type_expression(env, {:%{}, _meta, [{:+, update_from_ast} | args]}) do
  #   {env, update_from_unresolved} = parse_type_expression(env, update_from_ast)
  #   {env, update_from_resolved} = Type.get_type_or_ptr_type(env, update_from_unresolved)
  #   {env, update_from} = Type.App.get_type(env, update_from_resolved)
  #   case update_from do
  #     %Type.Record{} = record ->
  #       {env, typed_labels} =
  #         HMEnv.map_env(env, args, fn
  #           (env, {label, type_ast}) when is_atom(label) ->
  #             {env, type} = parse_type_expression(env, type_ast)
  #             {env, {label, type}}
  #         end)
  #       {env, type} = Type.Record.extend(env, record, typed_labels)
  #       {env, type}
  #     # %Type.Ptr.Unbound{} ->
  #     #   {env, type} = Type.Record.Unresolved.new()
  #     unhandled_type -> throw {:invalid_record_type, :not_record_type, unhandled_type, update_from_ast}
  #   end
  # end
  defp parse_type_expression(env, {:%{}, _meta, args}) do
    {env, typed_labels} =
      HMEnv.map_env(env, args, fn
        (env, {:+, type_ast}) ->
          {env, type_from_unresolved} = parse_type_expression(env, type_ast)
          {env, type_from_resolved} = Type.get_type_or_ptr_type(env, type_from_unresolved)
          {env, type_from} = Type.App.get_type(env, type_from_resolved)
          case type_from do
            %Type.Record{labels: labels, meta: _meta} -> {env, labels}
            unhandled_type -> throw {:invalid_record_type, :not_record_type, unhandled_type, type_ast}
          end
        (env, {:-, {label, _labelMeta, nil}}) -> {env, {:-, label}}
        (env, {label, type_ast}) when is_atom(label) ->
          {env, type} = parse_type_expression(env, type_ast)
          {env, {label, type}}
      end)
    typed_labels =
      typed_labels
      |> List.flatten()
      |> Enum.reduce([], fn
        ({:-, label}, a) -> Keyword.delete(a, label)
        (e, a) -> [e | a]
      end)
      |> :lists.reverse()
    {env, type} = Type.Record.new(env, typed_labels)
    {env, type}
  end
  # Named type
  defp parse_type_expression(env, {name, _meta, name_context}) when is_atom(name) and (name_context === nil or name_context === []) do
    key = Key.typename(name)
    case HMEnv.get_type(env, key) do
      nil -> throw {:TYPENAME_DOES_NOT_EXIST, name}
      type -> {env, type}
    end
  end
  # Applied named type
  defp parse_type_expression(env, {name, _meta, args}) when is_atom(name) and is_list(args) do
    key = Key.typename(name)
    case HMEnv.get_type(env, key) do
      nil -> throw {:TYPENAME_DOES_NOT_EXIST, name}
      %Type.App{} = type ->
        {env, args_types} = HMEnv.map_env(env, args, &parse_type_expression/2)
        {env, type} = Type.App.refine(env, type, args_types)
        {env, type}
      type ->
        throw {:unable_to_apply_to_type, type}
    end
  end
  defp parse_type_expression(_env, type_expr) do
    throw {:TODO, :unhandled_type_expression, type_expr}
  end


  defp wait_ensure_loaded(module, times \\ 20, sleep \\ 50)
  defp wait_ensure_loaded(module, times, _sleep) when times <= 0 do
    case Code.ensure_compiled(module) do
      {:module, ^module} -> {:module, module}
      _ -> throw {:unable_to_locate_module_named, module}
    end
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




  defp parse_module_type_enum(env, meta, args)
  # Enum declaration 0-arg
  defp parse_module_type_enum(env, meta, [{:=, _equals_meta, [{name, _name_meta, name_context}, {:enum, _enum_meta, flags}]}, cases])
  when (name_context === nil or name_context === []) and (flags === nil or is_list(flags)) do
    flags = List.wrap(flags)
    parse_module_type_enum(env, meta, name, flags, cases)
  end
  # enum-less helper
  defp parse_module_type_enum(env, meta, [{name, _name_meta, name_context}, cases]) when (name_context === nil or name_context === []) do
    flags = []
    parse_module_type_enum(env, meta, name, flags, cases)
  end
  defp parse_module_type_enum(_env, _meta, args) do
    throw {:TODO, :unhandled_parse_module_type_enum, args}
  end

  def add_atom_prefix(prefix, between \\ "_", atom) do
    prefix = to_string(prefix)
    between = to_string(between)
    atom = to_string(atom)
    String.to_atom(prefix <> between <> atom)
  end

  defp parse_module_type_enum(env, meta, name, flags, cases)
  defp parse_module_type_enum(env, meta, name, flags, cases) do
    {env, heads} = parse_module_type_enum_heads(env, meta, flags, cases)
    heads =
      case :long_name in flags do
        false -> heads
        true ->
          Enum.map(heads, fn
            {head_name, head_type} when is_atom(head_name) ->
              head_name = add_atom_prefix(name, head_name)
              {head_name, head_type}
            head_name when is_atom(head_name) ->
              add_atom_prefix(name, head_name)
          end)
      end
    {env, type} = Type.GADT.new(env, heads)
    key = Key.typename(name)
    env = HMEnv.push_type(env, key, type)
    env =
      case :no_register_heads in flags do
        true -> env
        false ->
          {env, _} =
            HMEnv.map_env(env, heads, fn
              (env, {head_name, head_type}) when is_atom(head_name) ->
                key = Key.enumeration(head_name)
                value = {head_type, type}
                env = HMEnv.push_type(env, key, value)
                call_type = {:enumeration_head, [type: type], [head_name, head_type]}
                env = add_call(env, head_name, call_type, type)
                {env, nil}
              (env, head_name) when is_atom(head_name) ->
                key = Key.enumeration(head_name)
                value = {nil, type}
                env = HMEnv.push_type(env, key, value)
                call_type = {:const, [type: type], head_name}
                env = add_call(env, head_name, call_type, type)
                {env, nil}
            end)
          env
      end
    ast = {:type, [type: type], [name, type]}
    {env, ast}
  end
  # defp parse_module_type_enum(_env, meta, name, flags, cases) do
  #   throw {:TODO, :unhandled_parse_module_type_enum_flags, meta, name, flags, cases}
  # end


  defp parse_module_type_enum_heads(env, meta, flags, cases)
  defp parse_module_type_enum_heads(env, _meta, flags, {:|, line_meta, [case_left, case_right]}) do
    {env, left} = parse_module_type_enum_heads(env, line_meta, flags, case_left)
    {env, right} = parse_module_type_enum_heads(env, line_meta, flags, case_right)
    heads = left ++ right
    {env, heads}
  end
  defp parse_module_type_enum_heads(env, _meta, _flags, {name, _name_meta, name_context}) when is_atom(name) and (name_context === nil or name_context === []) do
    head = name
    {env, [head]}
  end
  defp parse_module_type_enum_heads(env, _meta, flags, {name, _name_meta, [{:|, line_meta, [type_arg, cases]}]}) when is_atom(name) do
    {env, right} = parse_module_type_enum_heads(env, line_meta, flags, cases)
    {env, type} = parse_type_expression(env, type_arg)
    head = {name, type}
    heads = [head | right]
    {env, heads}
  end
  # defp parse_module_type_enum_heads(env, _meta, flags, {name, _name_meta, [{:|, line_meta, [{name2, _name_meta2, [type_arg]}, cases]}]}) when is_atom(name) do
  #   throw {:wha?, name, name2, type_arg, cases}
  #   {env, right} = parse_module_type_enum_heads(env, line_meta, flags, cases)
  #   {env, type} = parse_type_expression(env, type_arg)
  #   head = {name, type}
  #   heads = [head | right]
  #   {env, heads}
  # end
  defp parse_module_type_enum_heads(env, _meta, _flags, {name, _name_meta, [type_arg]}) when is_atom(name) do
    {env, type} = parse_type_expression(env, type_arg)
    head = {name, type}
    {env, [head]}
  end
  defp parse_module_type_enum_heads(_env, meta, flags, cases) do
    throw {:TODO, :unhandled_parse_module_type_enum_heads, meta, flags, cases}
  end




  defp parse_module_external(env, ext_meta, ext_args)
  # Externals *must* be typed
  defp parse_module_external(env, ext_meta, [{:|, _type_meta, [{name, _name_meta, args_ast}, {:=, _body_meta, [return_ast, body_ast]}]}]) when is_atom(name) do
    parse_module_external(env, ext_meta, name, args_ast, return_ast, body_ast)
  end
  defp parse_module_external(env, ext_meta, [{:|, _type_meta, [{name, _name_meta, args_ast}, return_ast]}, [do: body_ast]]) when is_atom(name) do
    parse_module_external(env, ext_meta, name, args_ast, return_ast, body_ast)
  end
  defp parse_module_external(_env, _ext_meta, ext_args) do
    throw {:TODO, :unhandled_external_expression, ext_args}
  end



  defp parse_module_external(env, meta, name, args_ast, return_ast, body_ast)
  defp parse_module_external(env, meta, name, args_ast, return_ast, body_ast) when is_atom(name) do
    env = HMEnv.push_scope(env, :def, name)
    {env, return_type} = parse_type_expression(env, return_ast)
    {env, args_types} = HMEnv.map_env(env, args_ast, &parse_type_expression/2)
    args_count = length(args_types)
    func_key = Key.func(name, args_count)
    {env, {modules, func_name}} = parse_module_external_body(env, body_ast)
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {env, args_types} = Type.generify_unbound(env, args_types)
    {env, func_type} = Type.Func.new(env, args_types, return_type, false, {modules, func_name, args_count})
    env = HMEnv.push_type(env, func_key, func_type)
    call_type = {:external_call, [type: func_type], [{modules, func_name, args_count}]}
    env = add_call(env, name, call_type, func_type)
    ast = {:external, [type: func_type] ++ meta, [name, {modules, func_name, args_count}]}
    {env, ast}
  end
  defp parse_module_external(_env, _meta, name, args_ast, return_ast, body_ast) do
    throw {:TODO, :unhandled_external_expression_impl, name, args_ast, return_ast, body_ast}
  end



  defp parse_module_external_body(env, body_ast)
  defp parse_module_external_body(env, {{:., _dot_meta, [{:__aliases__, _aliases_meta, modules}, func_name]}, _body_meta, []}) when is_list(modules) and is_atom(func_name) do
    {env, {modules, func_name}}
  end
  defp parse_module_external_body(_env, body_ast) do
    throw {:TODO, :unhandled_external_body, body_ast}
  end




  defp parse_module_def(env, meta, definition)
  # With type and body
  defp parse_module_def(env, meta, [{:|, _split_meta, [{name, _name_meta, args_ast}, {:=, _equals_meta, [type_expr, body_expr]}]}]) when is_atom(name) do
    {env, return_type} = parse_type_expression(env, type_expr)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
  end
  defp parse_module_def(env, meta, [{:|, _split_meta, [{name, _name_meta, args_ast}, type_expr]}, [do: body_expr]]) when is_atom(name) do
    {env, return_type} = parse_type_expression(env, type_expr)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
  end
  defp parse_module_def(_env, _meta, [{:|, _, _}|_]=definition) do
    throw {:TODO, :unhandled_typed_def_expression, definition}
  end
  # No type with body
  defp parse_module_def(env, meta, [{:=, _equals_meta, [{name, _name_meta, args_ast}, body_expr]}]) when is_atom(name) do
    {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
    parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr)
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
    {env, inner_type} = Type.Func.new(env, args_types, return_type, false, nil)
    func_key = Key.func(name, length(args))
    env = HMEnv.push_type(env, func_key, inner_type)
    {env, body} = parse_ml_expression(env, body_expr)
    {_, body_meta, _} = body
    body_type = body_meta[:type]
    {env, return_type} = Type.resolve_types!(env, body_type, return_type)
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {env, args_types} = Type.generify_unbound(env, args_types)
    {env, func_type} = Type.Func.new(env, args_types, return_type, false, nil)
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
    {env, type} = Type.Ptr.Unbound.new_ptr(env)
    key = Key.binding(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:binding, [type: type] ++ meta, name}
    {env, ast}
  end
  defp parse_module_def_head_arg(_env, arg) do
    throw {:TODO, :unhandled_def_head_arg, arg}
  end



  defp parse_ml_expression(env, expr)
  # Binding
  defp parse_ml_expression(env, {name, meta, context = nil}) when is_atom(name) and is_atom(context) do
    key = Key.binding(name)
    case HMEnv.get_type(env, key) do
      nil ->
        # Else check if there is an enumeration head exposed with this name
        key = Key.enumeration(name)
        case HMEnv.get_type(env, key) do
          nil -> throw {:BINDING_NOT_FOUND, name}
          {nil, type} ->
            ast = {:const, [type: type], name} # argumentless enum head
            {env, ast}
          {apply_type, type} ->
            ast = {:enumeration_head, [type: type], [name, apply_type]}
            {env, ast}
        end
      type ->
        ast = {:binding, [type: type] ++ meta, [name]}
        {env, ast}
    end
  end
  # Create record
  defp parse_ml_expression(env, {:%{}, meta, args_ast}) when is_list(args_ast) do
    {env, kwargs} =
      HMEnv.map_env(env, args_ast, fn(env, {label, ast}) ->
        {env, expr} = parse_ml_expression(env, ast)
        {env, {label, expr}}
      end)
    {env, kwtypes} =
      HMEnv.map_env(env, kwargs, fn(env, {label, {_, meta, _}}) ->
        type = meta[:type]
        {env, {label, type}}
      end)
    {env, type} = Type.Record.new(env, kwtypes)
    ast = {:record, [type: type] ++ meta, kwargs}
    {env, ast}
  end
  # Call 'something'
  defp parse_ml_expression(env, {name, meta, args}) when is_atom(name) and is_list(args) do
    key = Key.call(name)
    case HMEnv.get_type(env, key) do
      nil -> throw {:CALL_NOT_FOUND, name, env.types}
      {{:const, const_meta, value}, type} when args === [] ->
        ast = {:const, [type: type] ++ const_meta ++ meta, value}
        {env, ast}
      {{:enumeration_head, head_meta, [name, apply_type]}, type} ->
        case apply_type do
          %Type.Const{const: _const} ->
            case args do
              [] -> {env, {:enumeration_head, [type: type], [name, apply_type]}}
              [arg] ->
                {env, {_, arg_meta, _} = arg} = parse_ml_expression(env, arg)
                arg_type = arg_meta[:type]
                {env, _resolved_type} = Type.resolve_types!(env, arg_type, apply_type)
                {env, name_type} = Type.Const.new(env, :atom, values: [name])
                name_ast = {:const, [type: name_type], name}
                ast = {:enum_single, [type: type] ++ head_meta, [name_ast, arg]}
                {env, ast}
            end
          unhandled -> throw {:TODO, :unhandled_call_enumeration_type, unhandled}
        end
      {{:external_call, ext_meta, [{modules, func_name, arg_count}]}, type} ->
        case ext_meta[:type] do
          %Type.Func{args_types: args_types} ->
            {env, args_ast} = HMEnv.zipmap_env(env, args_types, args, fn
              (env, apply_type, arg) ->
                {env, {_, arg_meta, _} = arg_ast} = parse_ml_expression(env, arg)
                arg_type = arg_meta[:type]
                {env, _resolved_type} = Type.resolve_types!(env, arg_type, apply_type)
                {env, arg_ast}
            end)
            ast = {:call, [type: type] ++ ext_meta, [{modules, func_name, arg_count}, args_ast]}
            {env, ast}
        end
      unhandled -> throw {:TODO, :unhandled_call_type, args, unhandled}
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
    defmodule_ast
    # throw env.types
  end
  defp convert_to_elixir_module_ast(_env, name, name_meta, module_mlast, _module_type) do
    throw [:TODO, :convert_to_elixir_module_ast, name, name_meta, module_mlast]
  end



  defp generate_ml_module_info(_env, module_type) do
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
  defp convert_to_elixir_module_body_ast(_env, {:type, _, _}), do: nil # Ignore type definitions for per-expression code ast
  defp convert_to_elixir_module_body_ast(env, {:external, external_meta, [name, {modules, func_name, arg_count}]}) do
    type = external_meta[:type]
    args_types = type.args_types
    ^arg_count = length(args_types)
    # Go ahead and make a wrapper function for calling from non-typed code
    bindings =
      args_types
      |> Enum.with_index()
      |> Enum.map(&{:binding, [type: elem(&1,0)], [String.to_atom("#{func_name}__var__#{elem(&1,1)}")]})
    convert_to_elixir_module_body_ast(env, {:def, external_meta, [name, bindings, {:call, external_meta, [{modules, func_name, arg_count}, bindings]}]})
  end
  defp convert_to_elixir_module_body_ast(env, {:def, def_meta, [name, args, body_expr]}) when is_atom(name) and is_list(args) and not is_list(body_expr) do
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    name_ast = {name, [], args_ast}
    guards_ast =
      Enum.map(args, fn
        {:binding, m, _} = binding_ast ->
          bound_ast = convert_to_elixir_module_body_ast(env, binding_ast)
          case m[:type] do
            nil -> throw {:TODO, :AST_WITHOUT_TYPE, binding_ast}
            type ->
              case Type.get_type_or_ptr_type(env, type) do # TODO:  Type the guards?  Probably no point...
                {_env, %Type.Ptr.Generic{}} -> []
                {_env, %Type.Const{const: :integer}} -> {:is_integer, m, [bound_ast]}
                {_env, %Type.Const{const: :float}} -> {:is_float, m, [bound_ast]}
                {_env, %Type.Const{const: :atom}} -> {:is_atom, m, [bound_ast]}
                {_env, unhandled} -> throw {:TODO, :unhandled_arg_guard_type, unhandled, binding_ast}
              end
          end
        unhandled -> throw {:TODO, :unhandled_arg_guard, unhandled}
      end)
      |> List.flatten()
    when_ast =
      case guards_ast do
        [] -> name_ast
        guards -> # 'and' them all together...
          guards = Enum.reduce(guards, fn(left, right) -> {:and, [], [left, right]} end)
          {:when, [], [name_ast, guards]}
      end
    body_ast = convert_to_elixir_module_body_ast(env, body_expr)
    ast = {:def, def_meta, [when_ast, [do: body_ast]]}
    ast
  end
  defp convert_to_elixir_module_body_ast(env, {:call, external_meta, [{modules, func_name, arg_count}, args]}) do
    ^arg_count = length(args)
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {{:., [], [{:__aliases__, [alias: false], modules}, func_name]}, external_meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(_env, {:binding, meta, [name]}) when is_atom(name) do
    {name, meta, nil}
  end
  defp convert_to_elixir_module_body_ast(_env, {:binding, meta, name}) when is_atom(name) do
    {name, meta, nil}
  end
  # An unapplied enumeration head, make an anonymous function to hold it
  defp convert_to_elixir_module_body_ast(_env, {:enumeration_head, meta, [name, apply_type]}) do
    {:fn, fn_meta, fn_body} =
      case apply_type do
        %Type.Const{const: :integer} ->
          name_var = {name, [], nil}
          quote do
            fn (unquote(name_var)) when is_integer(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        %Type.Const{const: :float} ->
          name_var = {name, [], nil}
          quote do
            fn (unquote(name_var)) when is_float(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        %Type.Const{const: :atom} ->
          name_var = {name, [], nil}
          quote do
            fn (unquote(name_var)) when is_atom(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        type -> throw {:TODO, :unhandled_elixir_enumeration_type, type}
      end
    {:fn, meta ++ fn_meta, fn_body}
  end
  # single values enums shall be represented
  defp convert_to_elixir_module_body_ast(env, {:enum_single, _meta, [arg0, arg1]}) do
    arg0 = convert_to_elixir_module_body_ast(env, arg0)
    arg1 = convert_to_elixir_module_body_ast(env, arg1)
    {arg0, arg1}
  end
  # Records (in Elixir as maps)
  defp convert_to_elixir_module_body_ast(env, {:record, meta, args}) do
    args_ast = Enum.map(args, fn({label, arg}) -> {label, convert_to_elixir_module_body_ast(env, arg)} end)
    {:%{}, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(_env, {:const, _meta, value}), do: value
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
