defmodule TypedElixir do
  @moduledoc """
  """


  # TODO:  Make sure the return value of a function that ends in `?` only
  # returns a boolean()

  # TODO:  Walk the call path of the function and see what it returns and make
  # sure it matches the spec


  @fun_types [:def, :defp, :defmacro, :defmacrop]


  defmacro __using__(_opts) do
    quote bind_quoted: [] do
      import TypedElixir, only: [defmodulet: 2]
    end
  end

  defmacro defmodulet(alias_name, do: block_module) do
    quote do
      defmodule unquote(alias_name) do
        unquote(block_module)

        TypedElixir.type_check(
          unquote(alias_name),
          unquote(Macro.escape(block_module)),
          __ENV__
          )
      end
    end
  end


  def type_check(module_name, module_body, module_env)
  def type_check(module_name, body, env) do
    body = Macro.expand(body, env)
    IO.puts "Type Checking: #{inspect module_name}"
    # IO.inspect {module_name, body}#, env}
    # empty_context = %{
    #   types: %{},
    #   specs: %{}
    # }
    # type_check_module(module_name, body, env, empty_context)

    types = type_check_gather_types(module_name, body, env)
    IO.puts "\nTypes:"
    IO.inspect types

    specs = type_check_gather_specs(module_name, body, env)
    IO.puts "\nSpecs:"
    IO.inspect specs

    funs = type_check_gather_funs(module_name, body, env)
    IO.puts "\nFuns:"
    IO.inspect funs

    # check_fun_follows_spec_ = check_fun_follows_spec(module_name, body, env)
    # IO.puts "\ncheck_fun_follows_spec:"
    # IO.inspect check_fun_follows_spec_

    IO.puts "\n\n\n====== Actual Output ======\n"

    if funs !== %{} do
      funs
      |> Enum.filter(&(!Map.has_key?(specs, elem(&1,0))))
      |> Enum.map(fn {head, fun} -> "Fun missing specs: #{inspect head} -> #{inspect fun}" end)
      |> Enum.join("\n")
      |> IO.puts
    end

    # IO.puts "\n"
    #
    # check_fun_follows_spec_
    # |> Enum.join("\n")
    # |> IO.puts

    IO.puts "\n"

    # check_spec_fun_heads()

    nil
  end


  def type_check_gather_types(_module_name, {:__block__, _attrs, expressions}, _env) do
    for {:@, _, [{:type, _typeattrs, body_ast}]} <- expressions,
        [{:::, _, [{name_ast, _, nil}, type_ast]}] = body_ast,
        into: %{} do
      {name_ast, type_ast}
    end
  end


  def type_check_gather_specs(_module_name, {:__block__, _attrs, expressions}, _env) do
    for {:@, _, [{:spec, _specattrs, body_ast}]} <- expressions,
        [{:::, _, [{name_ast, _, func_def_ast}, func_ret_ast]}] = body_ast,
        name_arity = length(func_def_ast),
        into: %{} do
      {{name_ast, name_arity}, {func_def_ast, func_ret_ast}}
    end
  end


  def type_check_gather_funs(_module_name, {:__block__, _attrs, expressions}, _env) do
    funs = for expression <- expressions,
        {type, attrs, [fun_head_ast, fun_do_ast]} <- [expression],
        type in [:def, :defp, :defmacro, :defmacrop],
        name_ast = get_name_ast_from_head(fun_head_ast),
        {name, _, args_ast} = name_ast,
        name_arity = length(args_ast),
        defaults = Enum.filter_map(args_ast, fn
          {:'\\\\', _, _} -> true
          _ -> false
        end, fn {_, _, [arg_name, arg_default]} ->
          arg_default
        end),
        count_defaults = length(defaults),
        name_arity_min = name_arity - count_defaults
        # into: %{},
        do
          if count_defaults === 0 do
            {{name, name_arity}, expression}
          else
            [{{name, name_arity}, expression}
            | build_fun_defaults(type, attrs, name, :lists.reverse(args_ast), [])
              # for arity <- ((name_arity - 1) .. name_arity_min)
              #   do
              #     {{name, arity}, {type, attrs, []}
              #     }
              #   end
            ]
          end
        end
    funs |> List.flatten |> Enum.into(%{})
  end


  defp build_fun_defaults(type, attrs, name, head_ast_reversed, req_head)
  defp build_fun_defaults(type, attrs, name, [], req_head), do: []
  defp build_fun_defaults(type, attrs, name, [{:'\\\\', attr, default}|rest], req_head) do
    [{{name, length(rest) + length(req_head)}, {type, attr, [:delegate_default]}}
    | build_fun_defaults(type, attrs, name, rest, req_head)
    ]
  end
  defp build_fun_defaults(type, attrs, name, [req|rest], req_head) do
    build_fun_defaults(type, attrs, name, rest, [req|req_head])
  end


  def get_name_ast_from_head({:when, _, [name_ast|_]}), do: name_ast
  def get_name_ast_from_head(name_ast), do: name_ast

  def get_when_ast_from_head({:when, _, [_|when_ast]}), do: when_ast
  def get_when_ast_from_head(_), do: []


# def check_fun_follows_spec(module_name, {:__block__, _attrs, expressions}, _env) do
#   do_check_fun_follows_spec(module_name, expressions)
# end
#
# defp do_check_fun_follows_spec(module_name, expressions, spec \\ nil, funspec \\ nil)
# defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | expressions], nil, nil) when fun_type in @fun_types do
# IO.inspect "Blah1 #{inspect fun}"
#   [msg(mn, fun, "Fun did not have a spec immediately before it") |
#     do_check_fun_follows_spec(mn, expressions, nil, nil)]
# end
# defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | exprs], nil, funspec) when fun_type in @fun_types do
# IO.inspect "Blah2 #{inspect fun}"
#   case does_spec_match_fun_name(funspec, fun) do
#     nil -> do_check_fun_follows_spec(mn, exprs, nil, funspec)
#     err ->
#       [msg(mn, fun, "Fun does not match the prior listed spec with error of \"#{err}\"", funspec) |
#         do_check_fun_follows_spec(mn, exprs, nil, nil)]
#   end
# end
# defp do_check_fun_follows_spec(mn, [{fun_type, _, _} = fun | _exprs] = exprs, spec, _f) when fun_type in @fun_types do
# IO.inspect "Blah3 #{inspect fun}"
#   do_check_fun_follows_spec(mn, exprs, nil, spec)
# end
# defp do_check_fun_follows_spec(mn, [{:@, _, [{:spec, _specattrs, _body_ast}]} = newspec | expressions], nil, _fs) do
# IO.inspect "Blah4 #{inspect newspec}"
#   do_check_fun_follows_spec(mn, expressions, newspec, nil)
# end
# defp do_check_fun_follows_spec(mn, [{:@, _, [{:spec, _specattrs, _body_ast}]} = newspec | expressions], spec, nil) do
# IO.inspect "Blah5 #{inspect newspec}"
#   [msg(mn, spec, "Spec found when prior spec already stated without function", newspec) |
#     do_check_fun_follows_spec(mn, expressions, newspec, nil)]
# end
# defp do_check_fun_follows_spec(mn, [_e | expressions], nil, _fs) do
# IO.inspect "Blah6 #{inspect _e}"
#   do_check_fun_follows_spec(mn, expressions, nil, nil)
# end
# defp do_check_fun_follows_spec(mn, [_e | expressions], spec, _fs) do
# IO.inspect "Blah7 #{inspect _e}"
#   [msg(mn, spec, "Spec does not immediately preceed its function") |
#     do_check_fun_follows_spec(mn, expressions, nil, nil)]
# end
# defp do_check_fun_follows_spec(_mn, [], nil, _fs) do
# IO.inspect "Blah8"
#   []
# end
# defp do_check_fun_follows_spec(mn, [], spec, _fs) do
# IO.inspect "Blah9"
#   [msg(mn, spec, "Spec does not immediately preceed any function")]
# end


  @opaque spec_ast :: nil
  @opaque fun_ast :: nil

  @spec does_spec_match_fun_name(spec_ast, fun_ast) :: boolean() | nil
  defp does_spec_match_fun_name(spec, fun)
  defp does_spec_match_fun_name(
    {:@, _, [{:spec, spec_attrs, [{:::, _spec_attrs, [spec_head | spec_body]}]}]},
    {fun_type, fun_attrs, [fun_head | fun_body]})
  when fun_type in @fun_types do
    {spec_name, _spec_name_attr, spec_name_args} = get_name_ast_from_head(spec_head)
    {fun_name, _fun_name_attr, fun_name_args} = get_name_ast_from_head(fun_head)
IO.puts "blorp\n#{inspect spec_name}\n#{inspect fun_name}"
    if spec_name !== fun_name do
      "Names do not match of spec #{inspect spec_name} and fun #{inspect fun_name}"
    else
      # TODO:  Test arity here
      IO.puts "TODO: arity:\n#{inspect spec_name_args}\n#{inspect fun_name_args}"
      nil
    end
  end
  defp does_spec_match_fun_name(non_spec, non_fun) do
    "ERROR:  Not spec or fun:\nNonSpec: #{inspect non_spec}\nNonFun: #{inspect non_fun}"
  end


  def msg(module_name, issue_obj, msg, rel_issue_obj \\ nil)
  def msg(module_name, {_type, attr, _body} = issue_obj, msg, nil) do
    "#{msg(module_name, attr, msg)}\t#{inspect issue_obj}\n"
  end
  def msg(module_name, {_type, attr, _body} = issue_obj, msg, rel_issue_obj) do
    "#{msg(module_name, attr, msg)}\t#{inspect issue_obj}\n#{msg(module_name, rel_issue_obj, "Related")}"
  end
  def msg(module_name, [line: line], msg, _) do
    "#{module_name}:#{to_string line}: #{msg}\n"
  end
  def msg(module_name, nil, msg, _) do
    "#{module_name}: #{msg}\n"
  end
  def msg(module_name, issue_obj, msg, _) do
    "#{module_name}: #{msg}\n\tIssue Object: #{inspect issue_obj}\n"
  end

end
