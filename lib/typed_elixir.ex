defmodule TypedElixir do
  @moduledoc """
  """


  defmodule HMEnv do
    defstruct counter: -1, types: %{}, scopes: [], type_vars: %{}


    # Type helpers

    def push_type(env, name, ast) do
      types = Map.update(env.types, name, [ast], fn prior -> [ast | prior] end)
      env = %{env | types: types}
      env = add_to_scope(env, :type, name)
      env
    end

    def get_type(env, name) do
      case env.types[name] do
        nil -> throw {:GET_TYPE, name, "does not exist"}
        [] -> throw {:GET_TYPE, name, "does not exist"}
        [type | _] -> type
      end
    end

    defp pop_type(env, name) do
      types = case env.types[name] do
        nil -> throw {:POP_TYPE, name, "does not exist"}
        [] -> throw {:POP_TYPE, name, "does not exist"}
        [_] -> Map.delete(env.types, name)
        [_ | rest] -> Map.put(env.types, name, rest)
      end
      %{env | types: types}
    end


    # Type vars

    def push_type_var(env, name, id, type) do
      type_vars = Map.update(env.type_vars, name, [type], fn prior -> [type | prior] end)
      env = %{env | type_vars: type_vars}
      env = add_to_scope(env, :type_var, name)
      env
    end

    def get_type_var(env, id) do
      case env.type_vars[id] do
        nil -> throw {:GET_TYPE_VAR, id, "does not exist"}
        [] -> throw {:GET_TYPE_VAR, id, "does not exist"}
        [type_vars | _] -> type_vars
      end
    end

    defp pop_type_var(env, id) do
      type_vars = case env.type_vars[id] do
        nil -> throw {:POP_TYPE_VAR, id, "does not exist"}
        [] -> throw {:POP_TYPE_VAR, id, "does not exist"}
        [_] -> Map.delete(env.type_vars, id)
        [_ | rest] -> Map.put(env.type_vars, id, rest)
      end
      %{env | type_vars: type_vars}
    end


    # Scope helpers

    def push_scope(env, id) do
      scopes = [{id, []} | env.scopes]
      %{env | scopes: scopes}
    end

    def pop_scope(env) do
      [{id, scope} | scopes] = env.scopes
      env = %{env | scopes: scopes}
      env = Enum.reduce(scope, env, fn ({key, value}, env) -> removed_from_scope(env, key, value) end)
      {env, scope}
    end

    defp add_to_scope(env, key, val) do
      [{id, scope} | restScopes] = env.scopes
      scopes = [{id, [{key, val} | scope]} | restScopes]
      %{env | scopes: scopes}
    end

    defp removed_from_scope(env, key, val)
    defp removed_from_scope(env, :type, name), do: pop_type(env, name)
    defp removed_from_scope(env, :type_var, id), do: pop_type_var(env, id)
  end



  defmodule TypeConst         ,do: defstruct type: :"$$NO$TYPE$$", meta: %{}
  defmodule TypePtr           ,do: defstruct ptr: -1
  defmodule TypeCall          ,do: defstruct args: nil, return: nil # blah(args)
  defmodule TypeCallIndirect  ,do: defstruct args: nil, return: nil # blah.(args)




  defmacro __using__(_opts) do
    quote bind_quoted: [] do
      import TypedElixir, only: [defmodulet: 2]
    end
  end


  defp debug(val, opts, section, prefix \\ nil) do
    debugOpts = opts[:debug] || true #[:ALIAS] #[]
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

    block_module = typecheck_module(env, opts, alias_name, block_module)
      |> debug(opts, :TYPECHECKED)

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
  defp typecheck_module(env, opts, name, nil) when is_atom(name), do: nil
  defp typecheck_module(env, opts, name, {:__block__, meta, body_asts}) when is_atom(name) do
    env = HMEnv.push_scope(env, {:module, name})
    {env, bodies} = Enum.reduce(body_asts, {env, []}, fn(module_entry_ast, {env, bodies}) ->
      {env, body} = type_check_body(env, opts, module_entry_ast)
      {env, [body | bodies]}
    end)
    bodies = :lists.reverse(bodies)
  end



  defp type_check_body(env, opts, module_entry_ast)
  defp type_check_body(env, opts, {:def, meta, [head_ast, body_ast]}) do
    {env, head} = type_check_def_head(env, opts, head_ast)
    :TODO_BODY_DEF
    # ast = {:def, meta, [head, body]}
    # {env, ast}
  end
  defp type_check_body(env, opts, {:@, attrMeta, [attr_ast]}) do
    {env, attr} = type_check_attribute(env, opts, attr_ast)
    ast = {:@, attrMeta, [attr]}
    {env, ast}
  end



  defp type_check_attribute(env, opts, attr_ast)
  defp type_check_attribute(env, opts, {:spec, specMeta, [spec_ast]}) do
    {env, type} = parse_type_from_spec(env, opts, spec_ast)
    ast = {:spec, [type: type] ++ specMeta, [spec_ast]}
    {env, ast}
  end
  defp type_check_attribute(env, opts, attr_ast) do
    IO.inspect {:UNKNOWN_ATTRIBUTE, attr_ast}
    {env, attr_ast}
  end



  defp type_check_def_head(env, opts, head_ast)
  defp type_check_def_head(env, opts, {name, meta, args}) when is_atom(name) do
    env
    argCount = length(args)
    env = HMEnv.push_scope(env, {:DEF})
    case HMEnv.get_type(env, {name, argCount}) do
      nil -> throw {:TODO_DEF_HEAD_DEFNAME_INFER, name, meta, args}
      %TypeCall{} -> throw {:TODO_DEF_HEAD_DEFNAME_STATIC, name, meta, args}
      invalidType -> throw {:DEF_HEAD_DEFNAME, "Invalid type for name", name, meta, args, invalidType}
    end
  end







  defp parse_type_from_spec(env, opts, spec_ast)
  defp parse_type_from_spec(env, opts, {:::, meta, [{name, nameMeta, args_ast}, return_ast]}) when is_atom(name) do
    argCount = length(args_ast)
    {env, argsTypes} = Enum.reduce(args_ast, {env, []}, fn (arg_ast, {env, args}) ->
      {env, arg} = parse_type_from_spec_arg(env, opts, arg_ast)
      {env, [arg | args]}
      throw "TODO:  parse_type_from_spec  args"
    end)
    argsTypes = :lists.reverse(argsTypes)
    {env, returnType} = parse_type_from(env, opts, return_ast)
    type = %TypeCall{args: argsTypes, return: returnType}
    env = HMEnv.push_type(env, {name, argCount}, type)
    {env, type}
  end




  defp parse_type_from_spec_arg(env, opts, arg_ast)
  defp parse_type_from_spec_arg(env, opts, arg_ast) do
    # Use parse_type_from on each type?
    throw {:TODO_PARSE_SPEC_ARG, arg_ast}
  end

  defp parse_type_from(env, opts, type_ast)
  defp parse_type_from(env, opts, nil), do: {env, %TypeConst{type: nil}}
  defp parse_type_from(env, opts, type_ast) do
    throw {:TODO_PARSE_TYPE, type_ast}
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
