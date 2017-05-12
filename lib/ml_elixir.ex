defmodule MLElixir do
  @moduledoc """
  """


  # TODO:  BIG_TODO!  Refactor the error reporting from random throw's to actual exceptions that hold the ast meta information and such...


  alias TypedElixir.HMEnv
  alias TypedElixir.Type



  defmodule Key do
    def typename(name), do: {:typename, name}
    def func(name), do: {:func, name}
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


  import TypedElixir.HMEnv, only: [debug: 3, debug: 4, debug?: 2]



  defmacro defmlmodule(name, opts, [do: block_module]) do
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
  defmacro defmlmodule(name, [do: block_module]) do
    opts = Keyword.put([], :environment, __CALLER__)
# opts = Keyword.put(opts, :full_stack, true)
    case opts[:full_stack] do
      true -> defmlmodule_impl(name, block_module, opts)
      _ ->
        try do
          defmlmodule_impl(name, block_module, opts)
        catch
          x ->
            x =
              if debug?(opts, :plain_errors) do
                x
              else
                case x do
                  {:NO_TYPE_RESOLUTION, _, left, _, right} ->
                    left = Type.simple_description_of_type(left)
                    right = Type.simple_description_of_type(right)
                    "No Type Resolutions between `#{left}` and `#{right}`"
                  x -> x
                end
              end
            quote(do: throw unquote(x))
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

    module_output =
      case opts[:output_format] do
        elixir when elixir in [nil, :elixir, Elixir] ->
          module_output = convert_to_elixir_module_ast(env, name, name_meta, module_mlbody, module_type)
          if(debug?(env, :module_pretty_output), do: module_output |> Macro.to_string() |> IO.puts())
          module_output
        javascript when javascript in [:javascript, Javascript, :js, Js, JavaScript, JS] ->
          convert_to_javascript_module_text(env, name, name_meta, module_mlbody, module_type)
      end
      |> debug(env, :module_output)

    module_output
  end



  defp infer_module_type(env, module_mlbody) do
    {env, types} =
      Enum.reduce(module_mlbody, {env, %{}}, fn
        ({:type, meta, [name, _unresolved_type]}, {env, types}) ->
          {env, type} = Type.get_type_or_ptr_type(env, meta[:type])
          types = Map.put(types, name, type)
          {env, types}
        ({:def, _meta, [name, args, {_, body_meta, _}]}, {env, types}) ->
          {env, args_types} = HMEnv.map_env(env, args, fn(env, {_, meta, _}) -> {env, meta[:type]} end)
          return_type = body_meta[:type]
          is_indirect = false
          call = nil
          {env, type} = Type.Func.new(env, args_types, return_type, is_indirect, call)
          types = Map.put(types, name, type)
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
    {env, [name], [type: type]}
  end
  defp parse_defmlmodule_name_meta(env, {:__aliases__, meta, [name|_]=names}) when is_atom(name) do
    {env, type} = Type.Ptr.Unbound.new_ptr(env)
    {env, names, [type: type] ++ meta}
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
  # defp parse_module_expression(env, {:defmacro, defmacro_meta, defmacro_args}) do
  #   # parse_module_defmacro(env, defmacro_meta, defmacro_args)
  # end
  # defp parse_module_expression(env, {:letmacro, defmacro_meta, defmacro_args}) do
  #   # parse_module_defmacro(env, defmacro_meta, defmacro_args)
  # end
  # defp parse_module_expression(env, {:letmacro, defmacro_meta, defmacro_args}) do
  #   # parse_module_defmacro(env, defmacro_meta, defmacro_args)
  # end
  defp parse_module_expression(env, nil), do: {env, nil}
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
  # External module type
  defp parse_type_expression(env, {:__aliases__, _alias_meta, alias_args}) do
    module = Module.concat(alias_args)
    {:module, ^module} = wait_ensure_loaded(module)
    ml_module_info = module.ml_module_info()
    {env, ml_module_info.type} # The module_type
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
                nil -> throw {:NO_TYPE_ON_MODULE_TYPE, [line: dot_meta[:line]], name, type_name, types}
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
      # {[], %Type.Const{} = type} -> {env, type}
      # {[], %Type.Module{} = type} -> {env, type}
      {[], type} -> {env, type} # If no refinements then return the type directly
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
  defp parse_type_expression(env, {:{}, _meta, args}) do
    {env, elements} = HMEnv.map_env(env, args, &parse_type_expression/2)
    {env, type} = Type.Tuple.new(env, elements)
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
      nil -> throw {:TYPENAME_DOES_NOT_EXIST, name, args}
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
              (env, {head_name, %Type.Tuple{elements: []}}) when is_atom(head_name) ->
                key = Key.enumeration(head_name)
                value = {nil, type}
                env = HMEnv.push_type(env, key, value)
                call_type = {:enum, [type: type], [:tuple, head_name]}
                env = add_call(env, head_name, call_type, type)
                {env, nil}
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
                call_type = {:enum, [type: type], [:single, head_name]}
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
    func_key_global = Key.func(name)
    {env, {modules, func_name}} = parse_module_external_body(env, body_ast)
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {env, args_types} = Type.generify_unbound(env, args_types)
    {env, func_type} = Type.Func.new(env, args_types, return_type, false, {modules, func_name, args_count})
    env = HMEnv.push_type(env, func_key, func_type)
    env = HMEnv.push_type(env, func_key_global, func_type)
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
  # Macro invocation
  defp parse_module_def(env, meta, [{:&, _macrocall_meta, [{macro_name, _macro_meta, macro_args}]}]) do
    parse_module_macro_impl(env, meta, macro_name, macro_args)
  end
  defp parse_module_def(_env, _meta, definition) do
    throw {:TODO, :unhandled_def_expression, definition}
  end


  defp parse_module_macro_impl(env, meta, macro_name, macro_args)
  defp parse_module_macro_impl(env, meta, :macro, [{:|, type_meta, [{macro_name, macro_meta, macro_args}, {:=, equals_meta, [type_expr, body_expr]}]}]) when is_atom(macro_name) do
    macro_args = List.wrap(macro_args)
    {env, return_type} = parse_type_expression(env, type_expr)
    parse_module_def_impl(env, meta, macro_name, macro_args, return_type, body_expr)
    |> throw
  end
  defp parse_module_macro_impl(env, meta, :macro, [{:|, type_meta, [{macro_name, macro_meta, macro_args}, type_expr]}, [do: body_expr]]) do
    parse_module_macro_impl(env, meta, :macro, [{:|, macro_meta, [{macro_name, macro_meta, macro_args}, {:=, macro_meta, [type_expr, body_expr]}]}])
  end
  defp parse_module_macro_impl(env, meta, :macro, [{macro_name, macro_meta, macro_args}, [do: body_expr]]) do
    type_expr = {{:., meta, [{:__aliases__, meta, [:MLElixir, :MLMacro]}, :ast]}, meta, []} # Default this type
    parse_module_macro_impl(env, meta, :macro, [{:|, macro_meta, [{macro_name, macro_meta, macro_args}, {:=, meta, [type_expr, body_expr]}]}])
  end
  defp parse_module_macro_impl(env, meta, :macro, [{:=, equals_meta, [{macro_name, macro_meta, macro_args}, body_expr]}]) do
    type_expr = {{:., meta, [{:__aliases__, meta, [:MLElixir, :MLMacro]}, :ast]}, meta, []} # Default this type
    parse_module_macro_impl(env, meta, :macro, [{:|, macro_meta, [{macro_name, macro_meta, macro_args}, {:=, equals_meta, [type_expr, body_expr]}]}])
  end
  defp parse_module_macro_impl(_env, _meta, macro_name, macro_args) do
    throw {:TODO, :unhandled_macro_call, macro_name, macro_args}
  end


  defp parse_module_def_impl(env, meta, name, args, return_type, body_expr)
  defp parse_module_def_impl(env, meta, name, nil, return_type, body_expr), do: parse_module_def_impl(env, meta, name, [], return_type, body_expr)
  defp parse_module_def_impl(env, meta, name, args_ast, return_type, body_expr) when is_atom(name) and is_list(args_ast) do
    env = HMEnv.push_scope(env, :def, name)
    {env, args} = parse_module_def_args(env, args_ast)
    args |> debug(env, :DefArgs)
    args_types = Enum.map(args, fn {_, meta, _} -> meta[:type] end)
    {env, inner_type} = Type.Func.new(env, args_types, return_type, false, nil)
    func_key = Key.func(name, length(args))
    func_key_global = Key.func(name)
    env = HMEnv.push_type(env, func_key, inner_type)
    env = HMEnv.push_type(env, func_key_global, inner_type)
    {env, body} = parse_ml_expression(env, body_expr |> debug(false||env, :DefExpr, name))
    {_, body_meta, _} = body |> debug(false||env, :DefBody, name)
    body_type = body_meta[:type]
    {env, return_type} = Type.resolve_types!(env, body_type, return_type)
    return_type |> debug(false||env, :DefReturnType, name)
    {env, args_types} =
      HMEnv.map_env(env, args_types, fn (env, arg_type) -> Type.map_types(env, arg_type, fn(env, v) ->
        case Type.get_type_or_ptr_type(env, v) do
          {env, %Type.Ptr.Generic{}=gen} ->
            case Type.get_generic_bound(env, gen) do
              ^gen -> {env, v}
              actual_type -> {env, actual_type}
            end
          _res -> {env, v}
        end
      end) end)
    args_types |> debug(env, :DefArgsTypes)
    {env, args} = HMEnv.zipmap_env(env, args, args_types, fn(env, {binding, meta, args}, type) -> {env, {binding, [type: type]++meta, args}} end)
    {env, _scope} = HMEnv.pop_scope(env, :def)
    {args_types, args_types |> Enum.map(&elem(Type.get_type_or_ptr_type(env, &1), 1))} |> debug(env, :DefArgsTypesPre)
    {env, args_types} = Type.generify_unbound(env, args_types)
    {args_types, args_types |> Enum.map(&elem(Type.get_type_or_ptr_type(env, &1), 1))} |> debug(env, :DefArgsTypesPost)
    {env, func_type} = Type.Func.new(env, args_types, return_type, false, nil)
    env = HMEnv.push_type(env, func_key, func_type)
    env = HMEnv.push_type(env, func_key_global, func_type)
    ast = {:def, [type: func_type] ++ meta, [name, args, body]} |> debug(false||env, :DefAST, name)
if name in [:|>, :testering_op_pipe2] do
  # debug(ast, true, name, 1)
  # debug({func_type.args_types|>tl|>hd, Type.Ptr.get(env, func_type.args_types|>tl|>hd)}, true, name, 2)
  # debug({func_type.return_type, Type.Ptr.get(env, func_type.return_type)}, true, name, 3)
  # Process.sleep(50)
  # throw {}
end
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
  # Typed
  defp parse_module_def_head_arg(env, {:|, _meta, [expr, type_ast]}) do
    {env, {first, meta, last}} = parse_module_def_head_arg(env, expr)
    {env, type} = parse_type_expression(env, type_ast)
    {env, _resolved_type} = Type.resolve_types!(env, meta[:type], type)
    {env, {first, [type: type]++meta, last}}
  end
  # Match
  defp parse_module_def_head_arg(env, {:=, meta, [left_expr, right_expr]}) do
    {env, {_, left_meta, _} = left} = parse_module_def_head_arg(env, left_expr)
    {env, {_, right_meta, _} = right} = parse_module_def_head_arg(env, right_expr)
    {env, resolved_type} = Type.resolve_types!(env, left_meta[:type], right_meta[:type])
    {env, _resolved_type} = Type.resolve_types!(env, right_meta[:type], left_meta[:type])
    ast = {:match, [type: resolved_type]++meta, [left, right]}
    {env, ast}
  end
  # Match Tuple
  defp parse_module_def_head_arg(env, {:{}, meta, args}) do
    {env, args_asts} = HMEnv.map_env(env, args, &parse_module_def_head_arg/2)
    args_types = Enum.map(args_asts, &(elem(&1, 1)[:type]))
    {env, type} = Type.Tuple.new(env, args_types)
    ast = {:tuple, [type: type]++meta, args_asts}
    {env, ast}
  end
  # Match Record
  defp parse_module_def_head_arg(env, {:%{}, meta, args}) do
    {env, kwargs} =
      HMEnv.map_env(env, args, fn(env, {label, arg}) ->
        {env, arg} = parse_module_def_head_arg(env, arg)
        {env, {label, arg}}
      end)
    {env, kwtypes} =
      HMEnv.map_env(env, kwargs, fn(env, {label, {_, meta, _}}) ->
        type = meta[:type]
        {env, {label, type}}
      end)
    {env, type} = Type.Record.new(env, kwtypes)
    ast = {:record, [type: type]++meta, kwargs}
    {env, ast}
  end
  # Constants
  defp parse_module_def_head_arg(env, arg) when is_integer(arg) do
    {env, type} = Type.Const.new(env, :integer, values: [arg])
    {env, {:const, [type: type], arg}}
  end
  defp parse_module_def_head_arg(env, arg) when is_float(arg) do
    {env, type} = Type.Const.new(env, :float, values: [arg])
    {env, {:const, [type: type], arg}}
  end
  defp parse_module_def_head_arg(env, arg) when is_atom(arg) do
    {env, type} = Type.Const.new(env, :atom, values: [arg])
    {env, {:const, [type: type], arg}}
  end
  # Untyped Binding
  defp parse_module_def_head_arg(env, {name, meta, context = nil}) when is_atom(name) and is_atom(context) do
    {env, type} = Type.Ptr.Unbound.new_ptr(env)
    key = Key.binding(name)
    env = HMEnv.push_type(env, key, type)
    ast = {:binding, [type: type, binding: :head_arg] ++ meta, name}
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
          nil ->
            # Else check if there is a 0-arity function with this name
            key = Key.func(name)
            case HMEnv.get_type(env, key) do
              nil -> throw {:BINDING_NOT_FOUND, name, meta}
              %Type.Func{args_types: [], return_type: return_type} -> # Call it, 0-args
                ast = {:call, [type: return_type, binding: 1]++meta, [{name, 0}, []]}
                {env, ast}
              %Type.Func{} = type -> # Curry to pass an anonymous function
                # {env, type} = Type.unboundify_generic(env, type)
                {env, type} = Type.Func.unbind_generics(env, type)
                %Type.Func{args_types: args_types, return_type: return_type} = type
                args_types_length = length(args_types)
                {env, args_ast} =
                  HMEnv.zipmap_env(env, tl(Enum.into(-1..(args_types_length-1), [])), args_types, fn
                    (env, idx, type) ->
                      {env, var_id} = HMEnv.new_counter(env, :vars)
                      ast = {:binding, [type: type, binding: :ml_binding1]++meta, [String.to_atom("$var_#{idx}_#{var_id}")]}
                      {env, ast}
                  end)
                body_ast = {:call, [type: return_type, binding: 2]++meta, [{name, args_types_length}, args_ast]}
                ast = {:def, [type: type]++meta, [nil, args_ast, body_ast]}
                {env, ast}
            end
          {nil, %Type.GADT{heads: heads}=type} ->
            Enum.find_value(heads, nil, fn
              ^name ->
                ast = {:enum, [type: type], [:single, name]} # argumentless enum head
                {env, ast}
              {^name, %Type.Tuple{elements: []}} ->
                ast = {:enum, [type: type], [:tuple, name]} # argumentless enum tuple head
                {env, ast}
              {^name, value} -> throw {:blah, value}
              _ -> nil
            end)
            |> case do
              nil -> throw {:INVALID_GADT_HEAD, name, heads}
              result -> result
            end
          # {nil, type} ->
          #   ast = {:enum, [type: type], [:single, name]} # argumentless enum head
          #   {env, ast}
          {%Type.Const{}=apply_type, type} ->
            ast = {:enum, [type: type]++meta, [:incomplete, :value, name, [apply_type], []]}
            {env, ast}
          {%Type.Tuple{elements: elements}, type} ->
            ast = {:enum, [type: type]++meta, [:incomplete, :tuple, name, elements, []]}
            {env, ast}
          # {apply_type, type} ->
          #   ast = {:enumeration_head, [type: type], [name, apply_type]}
          #   {env, ast}
          {apply_type, type} -> throw {:TODO, :unhandled_gadt_enum_typing, apply_type}
        end
      type ->
        ast = {:binding, [type: type, binding: :ml_binding2] ++ meta, [name]}
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
  # Create tuple
  defp parse_ml_expression(env, {:{}, meta, args_ast}) when is_list(args_ast) do
    {env, args} = HMEnv.map_env(env, args_ast, &parse_ml_expression/2)
    args_types = Enum.map(args, &(elem(&1, 1)[:type]))
    {env, type} = Type.Tuple.new(env, args_types)
    ast = {:tuple, [type: type] ++ meta, args}
    {env, ast}
  end
  # Irritating Elixir, loses meta and all...
  defp parse_ml_expression(env, {left, right}), do: parse_ml_expression(env, {:{}, [], [left, right]})
  # Call external function
  defp parse_ml_expression(env, {{:., _dot_meta, [{:__aliases__, _alias_meta, module_names}=module_type_expr, name]}, call_meta, args}) do
    {env, type} = parse_type_expression(env, module_type_expr)
    {env, args_asts} = HMEnv.map_env(env, args, &parse_ml_expression/2)
    case type do
      %Type.Module{types: %{^name => %Type.Func{args_types: args_types, return_type: return_type}}} when length(args_types) === length(args) ->
        ast = {:call, [type: return_type, external: 1]++call_meta, [{module_names, name, length(args_types)}, args_asts]}
        {env, ast}
      %Type.Module{types: %{^name => %Type.GADT{heads: heads}=gadt}} ->
        {env, {:invalid, [type: gadt]++call_meta, []}}
      %Type.Module{types: types} -> throw {:MODULE_DOES_NOT_HAVE_FUNCTION, name, length(args), types}
      unknown_type -> throw {:NOT_A_MODULE_TYPE_WHEN_CALLING, name, unknown_type}
    end
  end
  # Call external 'something'
  defp parse_ml_expression(env, {{:., dot_meta, [called_expr, name]}, call_meta, args_expr}) when is_atom(name) do
    {env, args_asts} = HMEnv.map_env(env, args_expr, &parse_ml_expression/2)
    case parse_ml_expression(env, called_expr) do
      {env, {:invalid, meta, _}} -> # A Type, can we do something with it?
        case meta[:type] do
          %Type.GADT{heads: heads} = type ->
            args_length = length(args_asts)
            Enum.find_value(heads, nil, fn
              ^name when args_length === 0 ->
                ast = {:enum, [type: type]++call_meta, [:single, name]}
                {env, ast}
              {^name, %Type.Tuple{elements: []}} when args_length === 0 ->
                ast = {:enum, [type: type]++call_meta, [:tuple, name]}
                {env, ast}
              {^name, %Type.Tuple{elements: elements}} when args_length === length(elements) ->
                args_asts_types = Enum.map(args_asts, &(elem(&1, 1)[:type]))
                {env, _resolved_types} = HMEnv.zipmap_env(env, elements, args_asts_types, &Type.resolve_types!/3)
                ast = {:enum, [type: type]++call_meta, [:tuple, name | args_asts]}
                {env, ast}
              {^name, %Type.Const{}=head_arg_type} when args_length === 1 ->
                [{_, arg_meta, _}=arg_ast] = args_asts
                arg_type = arg_meta[:type]
                {env, _resolved_type} = Type.resolve_types!(env, head_arg_type, arg_type)
                ast = {:enum, [type: type]++call_meta, [:value, name, arg_ast]}
                {env, ast}
              {^name, head_arg_type} when args_length === 0 ->
                ast = {:enumeration_head, [type: type]++call_meta, [name, head_arg_type]}
                {env, ast}
              _ -> nil
            end)
            |> case do
              nil -> throw {:HEAD_NOT_FOUND_ON_GADT, name, args_length, heads}
              ast -> ast
            end
          unhandled_type -> throw {:TODO, :unhandled_dot_on_type, name, unhandled_type}
        end
      {env, {_, meta, _}=called_ast} -> # Callables
        {env, test_type} = Type.get_type_or_ptr_type(env, meta[:type])
        case test_type do
          %Type.Record{labels: labels} = called_type ->
            case labels[name] do
              nil -> throw {:INVALID_RECORD_KEY, name, called_type}
              value_type ->
                ast = {:record_access, [type: value_type]++dot_meta, [called_ast, name]}
                {env, ast}
            end
          unhandled_type -> throw {:TODO, :unhandled_call_on_type, unhandled_type, meta}
        end
    end

    # {env, {_, meta, _}=called_ast} = parse_ml_expression(env, called_expr)
    # called_type = meta[:type]
    # case called_type do
    #   %Type.Record{labels: labels} ->
    #     case labels[name] do
    #       nil -> throw {:INVALID_RECORD_KEY, name, called_type}
    #       value_type ->
    #         ast = {:record_access, [type: value_type]++dot_meta, [called_ast, name]}
    #         {env, ast}
    #     end
    #   unhandled_type -> throw {:TODO, :unhandled_call_on_type, unhandled_type}
    # end
  end
  # Call 'something'
  defp parse_ml_expression(env, {name, meta, args}) when is_atom(name) and is_list(args) do
    {name, meta, args} |> debug(false||env, :ExprCall)
    key = Key.call(name) |> debug(false||env, :ExprCall, :CallKey)
    case HMEnv.get_type(env, key) |> debug(false||env, :ExprCall, :CallKeyResult) do
      nil -> # Maybe it is a function?
        arg_mapper =
          fn
            (env, apply_type, {:_, _, scope}) when is_atom(scope) ->
              {env, {:new_binding, [type: apply_type, new_binding: 1]++meta, []}}
            (env, apply_type, {binding_name, _, scope}=arg) when is_atom(scope) ->
              case Atom.to_string(binding_name) do
                "_"<>test_name ->
                  case Integer.parse(test_name) do
                    {num, ""} when num>=0 and num<128 -> {env, {:new_binding, [type: apply_type, new_binding: 2]++meta, [num]}}
                    _ -> parse_ml_expression_with_type(env, apply_type, arg)
                  end
                _ -> parse_ml_expression_with_type(env, apply_type, arg)
              end
            (env, apply_type, arg) -> parse_ml_expression_with_type(env, apply_type, arg)
          end
        args_ast_reducer =
          fn
            ({:new_binding, m, []}, {env, args, calls, c}) ->
              {env, var_id} = HMEnv.new_counter(env, :vars)
              ast = {:binding, m, [String.to_atom("$var_#{c}_#{var_id}")]}
              {env, args} =
                if length(args) <= c do
                  {env, unbound} = Type.Ptr.Unbound.new_ptr(env)
                  {env, args++List.duplicate({:binding, [type: unbound, new_binding: 3], [:_]}, c-length(args)+1)}
                else
                  {env, args}
                end
              args = List.replace_at(args, c, ast)
              {env, args, calls++[ast], c+1}
            ({:new_binding, m, [c]}, {env, args, calls, _c}) ->
              {env, var_id} = HMEnv.new_counter(env, :vars)
              ast = {:binding, m, [String.to_atom("$var_#{c}_#{var_id}")]}
              {env, args} =
                if length(args) <= c do
                  {env, unbound} = Type.Ptr.Unbound.new_ptr(env)
                  {env, args++List.duplicate({:binding, [type: unbound, new_binding: 4], [:_]}, c-length(args)+1)}
                else
                  {env, args}
                end
              args = List.replace_at(args, c, ast)
              {env, args, calls++[ast], c+1}
            (arg_ast, {env, args, calls, c}) ->
              {env, args, calls++[arg_ast], c}
          end
        key = Key.func(name) |> debug(false||env, :ExprCall, :FuncKey)
        {env, type} =
          case HMEnv.get_type(env, key) |> debug(false||env, :ExprCall, :FuncKeyResult)do
            nil -> # TODO:  Maybe it is a callable variable?  Should probably test this first instead of last all things considered, so, it is a todo
              key = Key.binding(name) |> debug(false||env, :ExprCall, :BindingKey)
              base_type = HMEnv.get_type(env, key) |> debug(false||env, :ExprCall, :BindingKeyResult)
              {env, type} = Type.get_type_or_ptr_type(env, base_type)
              case type do
                nil -> throw {:CALL_NOT_FOUND, name, args, env.types}
                %Type.Ptr.Unbound{} -> # Unbound, and yet it is being called, refine it to a function
                  # First re-bind it to the function pointer of the same arity:
                  {env, args_types} =
                    HMEnv.map_env(env, args, fn(env, arg) ->
                      {env, {_, m, _,}} = parse_ml_expression(env, arg)
                      {env, m[:type]} # Just want the type to do the inferencing for
                    end)
                  # {env, return_type} = Type.Ptr.Generic.new_ptr(env, false)
                  {env, return_type} = Type.Ptr.Unbound.new_ptr(env)
                  {env, func} = Type.Func.new(env, args_types, return_type, true, nil)
                  {env, _resolved_func} = Type.resolve_types!(env, base_type, func |> debug(env, :ExprCall, :BindingKeyFuncType))
                  # debug({func.args_types|>hd, Type.get_type_or_ptr_type(env, func.args_types|>hd)|>elem(1)}, false||env, :Blargh, name)
                  {env, func}
                _ -> throw {:TODO, :unhandled_binding_call_type, type}
              end
            type ->
              {name, type, Type.debug_get_recursive_type_or_ptr_type(env, type)} |> debug(env, :ExprCall, :Bounded)
              # {env, type} = Type.unboundify_generic(env, type)
              {env, type} = Type.Func.unbind_generics(env, type)
              {name, type, Type.debug_get_recursive_type_or_ptr_type(env, type)} |> debug(env, :ExprCall, :Unbounded)
              {env, type}
          end
        case type do
          nil -> throw {:CALL_BINDING_NOT_FOUND, name, args, env.types}
          %Type.Func{} = type -> #when length(args_types) < length(args) -> # call function, then call the output of that function, etc..
            # args_length = length(args)
            {env, funcs_types} = Type.Func.get_func_return_list(env, type)
            args =
              if length(args) < length(type.args_types) do
                args ++ List.duplicate({:_, [], nil}, length(type.args_types)-length(args))
              else
                args
              end
            {[], funcs_types_reduced} =
              Enum.reduce(funcs_types, {args, []}, fn # Prune the funcs_types to only fit the argument count
                (%Type.Func{args_types: args_types}=type, {args, out}) when length(args_types) < length(args) -> {Enum.drop(args, length(args_types)), out++[type]}
                (_type, {[], out}) -> {[], out}
                (type, {_args, out}) -> {[], out++[type]}
              end)
            funcs_all_args_types = Enum.map(funcs_types_reduced, &Map.get(&1, :args_types)) |> List.foldr([], &Kernel.++/2)
            args =
              if length(args) < length(funcs_all_args_types) do
                args ++ List.duplicate({:_, [], nil}, length(funcs_all_args_types)-length(args))
              else
                args
              end
            {args, unused_args} = Enum.split(args, length(funcs_all_args_types))
            # {funcs_all_args_types, _unused_funcs_all_args_types} = Enum.split(funcs_all_args_types, args_length)
            # funcs_all_args_types = Enum.take(funcs_all_args_types, args_length)
            {env, args_ast} = HMEnv.zipmap_env(env, funcs_all_args_types, args, arg_mapper)
            {env, return, [nil], _main_args, _returning_type} =
              Enum.reduce(args_ast++[nil], {env, nil, [], [], type}, fn
                (argument, {env, prior_ast, args, main_args, %Type.Func{args_types: args_types, return_type: return_type, is_indirect: is_indirect}=type}) when length(args_types) === length(args) -> # Call or itemized curry
                  args_ast = :lists.reverse(args)
                  name_ast =
                    case prior_ast do
                      nil ->
                        if is_indirect do
                          [indirect: {:binding, [type: type, call_indirect: 1]++meta, [name]}]
                        else
                          {name, length(args)}
                        end
                      prior_ast -> {prior_ast}
                    end
                  {env, ast, more_args} =
                    case Enum.reduce(args_ast, {env, [], [], 0}, args_ast_reducer) do
                      {env, [], calls, _} ->
                        ast = {:call, [type: return_type, call: 1] ++ meta, [name_ast, calls]}
                        {env, ast, []}
                      {env, call_args, calls, _} ->
                        body_ast = {:call, [type: return_type, call: 2]++meta, [name_ast, calls]}
                        case argument do
                          nil ->
                            args_types = Enum.map(call_args, &(elem(&1, 1)[:type]))
                            {env, args_types} = Type.generify_unbound(env, args_types)
                            type = %{type | args_types: args_types}
                            ast = {:def, [type: type]++meta, [nil, main_args++call_args, body_ast]}# |> debug(name===:testering_func26d, name) # |>IO.inspect(label: name, syntax_colors: [number: :red, atom: :blue, tuple: :green, map: :yellow], width: 110, pretty: true)
                            {env, ast, []}
                          _ -> {env, body_ast, call_args}
                        end
                    end
                  {env, ast, [argument], main_args++more_args, return_type}
                (argument, {env, prior_ast, args, main_args, %Type.Func{args_types: args_types}=type}) when length(args) < length(args_types) -> # Not enough to call yet, add argument and reduce
                  {env, prior_ast, [argument | args], main_args, type}
                (nil, {env, ast, [nil], main_args, return_type}) ->
                  {env, ast, [nil], main_args, return_type}
              end)
            case unused_args do
              [] -> {env, return |> debug(env, :ExprCall, :Return)}
              unused_args -> throw {:attempted_to_pass_args_to_non_function, name, unused_args}
            end
        end
      {{:const, const_meta, value}, type} when args === [] ->
        ast = {:const, [type: type] ++ const_meta ++ meta, value}
        {env, ast}
      {{:enum, head_meta, args}, type} ->
        ast = {:enum, [type: type] ++ head_meta ++ meta, args}
        {env, ast}
      {{:enumeration_head, head_meta, [name | apply_type]}, type} ->
        case args do
          [] ->
            {:enumeration_head, [type: type], [name, apply_type]}
          [arg|_] ->
            {env, args_asts} = HMEnv.map_env(env, args, &parse_ml_expression/2)
            case apply_type do
              [%Type.Const{const: _const}=apply_type] when length(args)===1 ->
                {env, {_, arg_meta, _} = arg} = parse_ml_expression(env, arg)
                arg_type = arg_meta[:type]
                {env, _resolved_type} = Type.resolve_types!(env, arg_type, apply_type)
                # {env, name_type} = Type.Const.new(env, :atom, values: [name])
                # name_ast = {:const, [type: name_type], name}
                # ast = {:enum_single, [type: type] ++ head_meta, [name_ast, arg]}
                ast = {:enum, [type: type] ++ head_meta, [:value, name, arg]}
                {env, ast}
              [%Type.Tuple{elements: elements} | args_so_far] ->
                testing_args = args_so_far ++ args_asts
                testing_args_types = Enum.map(testing_args, &(elem(&1, 1)[:type]))
                testing_elements = Enum.take(elements, length(testing_args_types))
                remaining_elements = Enum.drop(elements, length(testing_args_types))
                {env, _resolved} = HMEnv.zipmap_env(env, testing_args_types, testing_elements, &Type.resolve_types!/3)
                ast =
                  if length(testing_args) === length(elements) do
                    {:enum, [type: type]++head_meta, [:tuple, name | testing_args]}
                  else
                    # if(name===:tuple_enum2, do: throw type)
                    {:enum, [type: type]++head_meta, [:incomplete, :tuple, name, remaining_elements, testing_args]}
                  end
                {env, ast}
              unhandled -> throw {:TODO, :unhandled_call_enumeration_type, unhandled}
            end
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
            ast = {:call, [type: type, call: 3] ++ ext_meta, [{modules, func_name, arg_count}, args_ast]}
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


  # When I rewrite all of this, thread return types through *all* of the AST generators too...
  defp parse_ml_expression_with_type(env, type, expr) do
    {env, {first, meta, last} = expr_ast} = parse_ml_expression(env, expr)
    inferred_type = meta[:type]
    {type, inferred_type, expr} |> debug(env, :ParseWithTyped)
    {env, from_inferred_type} = Type.get_type_or_ptr_type(env, inferred_type)
    {env, to_type} = Type.get_type_or_ptr_type(env, type)
# debug({from_inferred_type, to_type, expr}, true, :Varoop)
# Process.sleep(50)
# if(not first in [:const, :binding], do: throw({to_type, expr_ast}))
    case {from_inferred_type, to_type} do
      {%Type.Func{is_indirect: false, args_types: inferred_args_types}, %Type.Func{is_indirect: true, args_types: args_types, return_type: return_type}} when length(inferred_args_types) === length(args_types) ->
        new_type = %{inferred_type | is_indirect: true}
        {type, new_type} |> debug(env, :ParseWithTyped, :typed)
        {env, resolved_type} = Type.resolve_types!(env, type, new_type)
        resolved_type |> debug(env, :ParseWithTyped, :resolved)
        {env, args_ast} = HMEnv.map_env(env, args_types, fn(env, type) ->
          {env, var_id} = HMEnv.new_counter(env, :vars)
          ast = {:binding, [type: type, call_type: 1]++meta, [String.to_atom("$var_#{var_id}")]}
          {env, ast}
        end)
        body_ast = {:call, [type: return_type, call_type: 1] ++ meta, [{expr_ast}, args_ast]}
        ast = {:def, [type: resolved_type]++meta, [nil, args_ast, body_ast]}
        {env, ast}
      {%Type.Func{is_indirect: false, args_types: inferred_args_types}, %Type.Func{is_indirect: true, args_types: args_types, return_type: _return_type}} when length(inferred_args_types) > length(args_types) ->
        # Passed in function has more args than request, make a wrapper that return a function that takes the rest
        {wrapper_args, wrapped_args} = Enum.split(inferred_args_types, length(args_types))
        wrapped_type = %{from_inferred_type | is_indirect: true, args_types: wrapped_args}
        wrapper_type = %{from_inferred_type | is_indirect: true, args_types: wrapper_args, return_type: wrapped_type}
        {env, resolved_type} = Type.resolve_types!(env, type, wrapper_type)
        {env, wrapped_args_ast} = HMEnv.zipmap_env(env, wrapped_args, args_types, fn(env, type, become_type) ->
          {env, resolved_type} = Type.resolve_types!(env, type, become_type)
          {env, var_id} = HMEnv.new_counter(env, :vars)
          ast = {:binding, [type: resolved_type, call_type: 2]++meta, [String.to_atom("$var_#{var_id}")]}
          {env, ast}
        end)
        {env, wrapper_args_ast} = HMEnv.map_env(env, wrapper_args, fn(env, type) ->
          {env, var_id} = HMEnv.new_counter(env, :vars)
          ast = {:binding, [type: type, call_type: 3]++meta, [String.to_atom("$var_#{var_id}")]}
          {env, ast}
        end)
        wrapped_body_ast = {:call, [type: wrapped_type.return_type, call_type: 2] ++ meta, [{expr_ast}, wrapped_args_ast]}
        wrapped_ast = {:def, [type: wrapped_type]++meta, [nil, wrapped_args_ast, wrapped_body_ast]}
        wrapper_body_ast = wrapped_ast
        wrapper_ast = {:def, [type: wrapped_type]++meta, [nil, wrapper_args_ast, wrapper_body_ast]}
        {env, wrapper_ast}
        debug(wrapper_ast, true, :Blargh)
        Process.sleep(100)
        throw {:TODO, :make_function_wrapper_of_proper_length, wrapper_type, to_type, wrapper_ast}
      _ ->
        {env, resolved_type} = Type.resolve_types!(env, type, inferred_type)
        {type, inferred_type, [type, inferred_type] |> Type.debug_get_recursive_type_or_ptr_type(env)} |> debug(env, :ParseWithTyped, :Unknown)
        {env, {first, [type: resolved_type, unknown: :call1]++meta, last}}
    end
  end


  defp block_if_not_block(ast)
  defp block_if_not_block({:__block__, meta, exprs}=ast) when is_list(meta) and is_list(exprs), do: ast
  # defp block_if_not_block(asts) when is_list(asts), do: {:__block__, [], asts}
  defp block_if_not_block(ast), do: {:__block__, [], [ast]}




  defp convert_to_elixir_module_ast(env, names, name_metae, module_mlast, module_type)
  defp convert_to_elixir_module_ast(env, [name|_]=names, name_meta, module_mlasts, module_type) when is_atom(name) and is_list(module_mlasts) do
    name_ast = {:__aliases__, [alias: false], names}
    body_ast = Enum.map(module_mlasts, &convert_to_elixir_module_body_ast(env, &1)) |> Enum.filter(&(&1))
    body_ast =
      [ {:import, [], [{:__aliases__, [], [:Kernel]}, [
          # only: [
          #   def: 2,
          #   is_integer: 1, is_float: 1, is_atom: 1, is_tuple: 1, is_function: 2, is_map: 1,
          #   and: 2, or: 2
          # ]]]} # TODO:  Add more exceptions from Kernel here
          except: [|>: 2]]]} # TODO:  Add more exceptions from Kernel here
        | body_ast
      ]
    ml_module_info_ast = generate_ml_module_info(env, module_type)
    modulebody_ast = [name_ast, [do: {:__block__, [], body_ast ++ ml_module_info_ast}]]
    defmodule_ast = {:defmodule, [context: __MODULE__, import: Kernel] ++ name_meta, modulebody_ast}
    defmodule_ast
    # throw env.types
  end
  defp convert_to_elixir_module_ast(_env, names, name_meta, module_mlast, _module_type) do
    throw [:TODO, :convert_to_elixir_module_ast, names, name_meta, module_mlast]
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



  defp convert_to_elixir_guard(env, typed_ast)
  defp convert_to_elixir_guard(env, {:binding, m, _} = binding_ast) do
    bound_ast = convert_to_elixir_module_body_ast(env, binding_ast)
    case m[:type] do
      nil -> throw {:TODO, :AST_WITHOUT_TYPE, binding_ast}
      type ->
        case Type.get_type_or_ptr_type(env, type) do # TODO:  Type the guards?  Probably no point...
          {_env, %Type.Ptr.Generic{}} -> []
          {_env, %Type.Const{const: :integer}} -> {:is_integer, m, [bound_ast]}
          {_env, %Type.Const{const: :float}} -> {:is_float, m, [bound_ast]}
          {_env, %Type.Const{const: :atom}} -> {:is_atom, m, [bound_ast]}
          {_env, %Type.Record{labels: _labels}} -> {:is_map, m, [bound_ast]} # TODO:  Add a matching context for this
          {_env, %Type.Tuple{elements: elements}} ->
            [
              {:is_tuple, m, [bound_ast]},
              {:===, m, [
                {:tuple_size, m, [bound_ast]},
                length(elements)
              ]} # TODO:  Recursively test through a tuple, need to pull out the guard generator into another function to be able to do that...
            ]
          {_env, %Type.Func{is_indirect: true, args_types: args_types}} ->
            debug(type, env, :Indirect)
            {:is_function, m, [bound_ast, length(args_types)]}
          {_env, unhandled} -> debug({unhandled, binding_ast}, true, :Whaaaa); []
          {_env, unhandled} -> throw {:TODO, :unhandled_arg_guard_type, unhandled, binding_ast}
        end
    end
  end
  defp convert_to_elixir_guard(_env, {:const, _m, _value}) do
    # No need to build a guard for a constant type...
    []
  end
  defp convert_to_elixir_guard(env, {:match, _m, [left, right]}) do
    left_guard = convert_to_elixir_guard(env, left)
    right_guard = convert_to_elixir_guard(env, right)
    [left_guard, right_guard] # Eh, match everything for now, can refine later...
  end
  defp convert_to_elixir_guard(env, {:record, _meta, elements}) do
    Enum.map(elements, &convert_to_elixir_guard(env, elem(&1, 1)))
  end
  defp convert_to_elixir_guard(env, {:tuple, _meta, elements}) do
    Enum.map(elements, &convert_to_elixir_guard(env, &1))
  end
  defp convert_to_elixir_guard(_env, typed_ast) do
    throw {:TODO, :unhandled_arg_guard, typed_ast}
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
      |> Enum.map(&{:binding, [type: elem(&1,0)], [String.to_atom("$var_#{elem(&1,1)}")]})
    convert_to_elixir_module_body_ast(env, {:def, external_meta, [name, bindings, {:call, external_meta, [{modules, func_name, arg_count}, bindings]}]})
  end
  defp convert_to_elixir_module_body_ast(env, {:def, def_meta, [name, args, body_expr]}) when is_atom(name) and is_list(args) and not is_list(body_expr) do
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    guards_ast =
      if env.user[:disable_guards] do
        []
      else
        Enum.map(args, &convert_to_elixir_guard(env, &1))
        |> List.flatten()
      end
    body_ast = convert_to_elixir_module_body_ast(env, body_expr)
    case name do
      nil -> nil
        when_ast =
          case guards_ast do
            [] -> args_ast
            guards -> # 'and' them all together...
              guards = Enum.reduce(guards, fn(left, right) -> {:and, [], [left, right]} end)
              [{:when, [], args_ast++[guards]}]
          end
        ast = {:fn, def_meta, [ {:->, [], [when_ast, body_ast]} ]}
        ast
      name ->
        name_ast = {name, [], args_ast}
        when_ast =
          case guards_ast do
            [] -> name_ast
            guards -> # 'and' them all together...
              guards = Enum.reduce(guards, fn(left, right) -> {:and, [], [left, right]} end)
              {:when, [], [name_ast, guards]}
          end
        ast = {:def, def_meta, [when_ast, [do: body_ast]]}
        ast
    end
  end
  # defp convert_to_elixir_module_body_ast(env, {:curry, meta, [{func_name, arg_count}, args]}) do
  #   %Type.Func{args_types: args_types} = meta[:type]
  #   {head_ast, guard_ast, args_ast} =
  #     Enum.zip(args, args_types)
  #     Enum.reduce({[], [], []}, fn
  #       ({nil, type}, {head, guard, args}) ->
  #         head = head ++
  #         {head, args}
  #       ({idx, type}, {head, guard, args}) when is_integer(idx) -> throw "Not handled yet 'idx'"
  #       ({ast, type}, {head, guard, args}) -> throw "Not handled yet 'ast'"
  #     end)
  # end
  defp convert_to_elixir_module_body_ast(env, {:call, meta, [{modules, func_name, arg_count}, args]}) do
    ^arg_count = length(args)
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {{:., [], [{:__aliases__, [alias: false], modules}, func_name]}, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(env, {:call, meta, [{func_name, arg_count}, args]}) do
    ^arg_count = length(args)
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {func_name, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(env, {:call, meta, [{prior}, args]}) do
    prior_ast = convert_to_elixir_module_body_ast(env, prior)
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {{:., [], [prior_ast]}, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(env, {:call, meta, [[indirect: callee], args]}) do
    callee = convert_to_elixir_module_body_ast(env, callee)
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {{:., [], [callee]}, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(_env, {:binding, meta, [name]}) when is_atom(name) do
    {name, meta, nil}
  end
  defp convert_to_elixir_module_body_ast(_env, {:binding, meta, name}) when is_atom(name) do
    {name, meta, nil}
  end
  # An unapplied enumeration head, make an anonymous function to hold it
  defp convert_to_elixir_module_body_ast(_env, {:enumeration_head, meta, [name, apply_type]}) do
    name_var = {name, [], nil}
    {:fn, fn_meta, fn_body} =
      case apply_type do
        %Type.Const{const: :integer} ->
          quote do
            fn (unquote(name_var)) when is_integer(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        %Type.Const{const: :float} ->
          quote do
            fn (unquote(name_var)) when is_float(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        %Type.Const{const: :atom} ->
          quote do
            fn (unquote(name_var)) when is_atom(unquote(name_var)) -> {unquote(name), unquote(name_var)} end
          end
        type -> throw {:TODO, :unhandled_elixir_enumeration_type, name, type}
      end
    {:fn, meta ++ fn_meta, fn_body}
  end
  # Enum values
  defp convert_to_elixir_module_body_ast(env, {:enum, meta, enum_args}) do
    case enum_args do
      [:single, name] -> name
      [:value, name, value] -> {name, convert_to_elixir_module_body_ast(env, value)}
      [:tuple, name | args] -> {:{}, meta, [name | Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))]}
      [:incomplete, type, name, args, already_complete] ->
        already_complete_args_ast = Enum.map(already_complete, &convert_to_elixir_module_body_ast(env, &1))
        {args_ast, guards_ast, _counter} = Enum.reduce(args, {[], [], 0}, fn (v, {args, guards, counter}) ->
          arg =
            Macro.var(String.to_atom("$var_#{to_string(counter)}"), nil)
          guard =
            case v do
              %Type.Const{const: :float} -> quote(do: is_float(unquote(arg)))
              %Type.Const{const: :integer} -> quote(do: is_integer(unquote(arg)))
              %Type.Const{const: :atom} -> quote(do: is_atom(unquote(arg)))
              _v -> []
            end
            |> List.wrap()
          {args ++ [arg], guard ++ guards, counter+1}
        end)
        head_ast =
          case guards_ast do
            [] -> args_ast
            _ ->
              guards_ast = Enum.reduce(guards_ast, fn(prior, next) -> {:and, [], [prior, next]} end)
              [{:when, [], args_ast ++ [guards_ast]}]
          end
        body_ast =
          case type do
            :value when length(args_ast)===1 -> {:{}, meta, [name | args_ast]}
            :tuple -> {:{}, meta, [name] ++ already_complete_args_ast ++ args_ast}
            v -> throw {:TODO, :unhandled_enum_body_type, v, length(args_ast)}
          end
        ast = {:fn, [], [{:->, [], [head_ast, body_ast]}]}
        ast
    end
  end
  # single values enums shall be represented
  # defp convert_to_elixir_module_body_ast(env, {:enum_single, _meta, [arg0, arg1]}) do
  #   arg0 = convert_to_elixir_module_body_ast(env, arg0)
  #   arg1 = convert_to_elixir_module_body_ast(env, arg1)
  #   {arg0, arg1}
  # end
  # Records (in Elixir as maps)
  defp convert_to_elixir_module_body_ast(env, {:record, meta, args}) do
    args_ast = Enum.map(args, fn({label, arg}) -> {label, convert_to_elixir_module_body_ast(env, arg)} end)
    {:%{}, meta, args_ast}
  end
  defp convert_to_elixir_module_body_ast(env, {:record_access, meta, [record, name]}) when is_atom(name) do
    record_ast = convert_to_elixir_module_body_ast(env, record)
    {{:., meta, [:maps, :get]}, meta, [name, record_ast]}
  end
  # Tuples
  defp convert_to_elixir_module_body_ast(env, {:tuple, meta, args}) do
    args_ast = Enum.map(args, &convert_to_elixir_module_body_ast(env, &1))
    {:{}, meta, args_ast}
  end
  # Match
  defp convert_to_elixir_module_body_ast(env, {:match, meta, [left, right]}) do
    left_ast = convert_to_elixir_module_body_ast(env, left)
    right_ast = convert_to_elixir_module_body_ast(env, right)
    {:=, meta, [left_ast, right_ast]}
  end
  defp convert_to_elixir_module_body_ast(_env, {:const, _meta, value}), do: value
  defp convert_to_elixir_module_body_ast(_env, module_expr) do
    throw {:TODO, :unhandled_module_body_expr, module_expr}
  end



  ### Javascript output

  defp convert_to_javascript_module_text(env, names, name_metae, module_mlast, module_type)
  defp convert_to_javascript_module_text(env, [name|_]=names, _name_meta, module_mlasts, module_type) when is_atom(name) and is_list(module_mlasts) do
    _ml_module_info_ast = generate_ml_module_info(env, module_type)
    [
      "// Module Name:  ", Enum.join(names, "."), ?\n,
      "// Version: 0.0.1", ?\n, ?\n,
      Enum.map(module_mlasts, &convert_to_javascript_module_body_ast(env, &1)),
    ]
  end
  defp convert_to_javascript_module_text(_env, name, name_meta, module_mlast, _module_type) do
    throw [:TODO, :convert_to_javascript_module_ast, name, name_meta, module_mlast]
  end


  defp escape_js_string(atom) when is_atom(atom), do: escape_js_string(to_string(atom))
  defp escape_js_string(string) when is_binary(string) do
    string
  end

  defp create_js_string(value) do
    [?", escape_js_string(value), ?"]
  end


  defp convert_to_javascript_module_body_ast(env, module_expr)
  defp convert_to_javascript_module_body_ast(_env, {:type, _, _}), do: []
  defp convert_to_javascript_module_body_ast(_env, {:const, _meta, value}) when is_atom(value), do: [?', to_string(value), ?']
  defp convert_to_javascript_module_body_ast(_env, {:const, _meta, value}), do: to_string(value)
  defp convert_to_javascript_module_body_ast(_env, {:binding, _meta, [name]}) when is_atom(name) do
    to_string(name)
  end
  defp convert_to_javascript_module_body_ast(_env, {:binding, _meta, name}) when is_atom(name) do
    to_string(name)
  end
  # Records (in javascript as objects)
  defp convert_to_javascript_module_body_ast(env, {:record, _meta, args}) do
    [
      ?{,
      Enum.map(args, fn({label, arg}) -> [?", escape_js_string(label), "\": ", convert_to_javascript_module_body_ast(env, arg)] end) |> Enum.intersperse(", "),
      ?},
    ]
  end
  defp convert_to_javascript_module_body_ast(env, {:call, _external_meta, [{modules, func_name, _arg_count}, args]}) do
    modules =
      case modules do
        [] -> ""
        [required | rest] -> ["require(\"", escape_js_string(required), "\")." | Enum.map(rest, &[escape_js_string(&1), ?.])]
      end
    [
      modules, to_string(func_name), ?(,
      Enum.map(args, &convert_to_javascript_module_body_ast(env, &1)) |> Enum.intersperse(", "), ?)
    ]
  end
  defp convert_to_javascript_module_body_ast(env, {:external, external_meta, [name, {modules, func_name, arg_count}]}) do
    type = external_meta[:type]
    args_types = type.args_types
    ^arg_count = length(args_types)
    # Go ahead and make a wrapper function for calling from non-typed code
    bindings =
      args_types
      |> Enum.with_index()
      |> Enum.map(&{:binding, [type: elem(&1,0)], [String.to_atom("#{func_name}__var__#{elem(&1,1)}")]})
    convert_to_javascript_module_body_ast(env, {:def, external_meta, [name, bindings, {:call, external_meta, [{modules, func_name, arg_count}, bindings]}]})
  end
  defp convert_to_javascript_module_body_ast(env, {:def, _def_meta, [name, args, body_expr]}) when is_atom(name) and is_list(args) and not is_list(body_expr) do
    [
      "function ", to_string(name), "(", Enum.map(args, &convert_to_javascript_module_body_ast(env, &1)) |> Enum.intersperse(", "), ") {", ?\n,
      ?\t, "return ", convert_to_javascript_module_body_ast(env, body_expr), ?;, ?\n,
      ?}, ?\n,
    ]
  end
  # Enums
  defp convert_to_javascript_module_body_ast(env, {:enum, _meta, [enum_type, name | enum_args]}) do
    name_ast = create_js_string(name)
    case [enum_type | enum_args] do
      [:single] -> name_ast
      [:value, value] -> [?[, name_ast, ", ", convert_to_javascript_module_body_ast(env, value), ?]]
      [:incomplete, type, args, already_complete] ->
        already_complete_args_ast =
          Enum.map(already_complete, &convert_to_javascript_module_body_ast(env, &1))
          |> Enum.intersperse(", ")
          |> case do
            [] -> []
            list -> [list, ", "]
          end
        args_ast =
          Enum.with_index(args)
          |> Enum.map(fn{_type, counter} -> "$var_#{counter}" end)
          |> Enum.intersperse(", ")
        [
          "function ", to_string(name), ?(, args_ast, "){return",
          case type do
            :integer ->
              [
                ?[, to_string(name), ", ",
                already_complete_args_ast, args_ast, ?]
              ]
            :tuple ->
              [
                ?[, to_string(name), ", ",
                already_complete_args_ast, args_ast, ?]
              ]
          end,
          ";}",
        ]
    end
  end
  # An unapplied enumeration head, make an anonymous function to hold it
  defp convert_to_javascript_module_body_ast(_env, {:enumeration_head, _meta, [name, _apply_type]}) do
    [ "function(__ANON_ARG__){return['",
      to_string(name),
      "', __ANON_ARG__];}",
    ]
  end
  defp convert_to_javascript_module_body_ast(_env, module_expr) do
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
