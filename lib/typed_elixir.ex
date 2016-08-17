defmodule TypedElixir do
  @moduledoc """
  """


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
  def type_check(name, body, env) do
    body = Macro.expand(body, env)
    IO.puts "Type Checking: #{inspect name}"
    # IO.inspect {name, body}#, env}
    # empty_context = %{
    #   types: %{},
    #   specs: %{}
    # }
    # type_check_module(name, body, env, empty_context)

    types = type_check_gather_types(name, body, env)
    IO.puts "\nTypes:"
    IO.inspect types

    specs = type_check_gather_specs(name, body, env)
    IO.puts "\nSpecs:"
    IO.inspect specs

    funs = type_check_gather_funs(name, body, env)
    IO.puts "\nFuns:"
    IO.inspect funs

    IO.puts "\nFuns missing specs:"
    funs
    |> Enum.filter(&(!Map.has_key?(specs, elem(&1,0))))
    |> IO.inspect

    nil
  end


  def type_check_gather_types(_name, {:__block__, _attrs, expressions}, _env) do
    for {:@, _, [{:type, _typeattrs, body_ast}]} <- expressions,
        [{:::, _, [{name_ast, _, nil}, type_ast]}] = body_ast do
      {name_ast, type_ast}
    end
    |> Enum.into(%{})
  end


  def type_check_gather_specs(_name, {:__block__, _attrs, expressions}, _env) do
    for {:@, _, [{:spec, _specattrs, body_ast}]} <- expressions,
        [{:::, _, [{name_ast, _, func_def_ast}, func_ret_ast]}] = body_ast,
        name_arity = length(func_def_ast) do
      {{name_ast, name_arity}, {func_def_ast, func_ret_ast}}
    end
    |> Enum.into(%{})
  end


  def type_check_gather_funs(_name, {:__block__, _attrs, expressions}, _env) do
    expressions
    |> Enum.filter(fn
      {:def, _, _} -> true
      {:defp, _, _} -> true
      {:defmacro, _, _} -> true
      {:defmacrop, _, _} -> true
      _ -> false
    end)
    for {type, _, [fun_head_ast, fun_do_ast]} <- expressions,
        type in [:def, :defp, :defmacro, :defmacrop],
        name_ast = (case fun_head_ast do
          {:when, _, [name_ast|_]} -> name_ast
          name_ast -> name_ast
        end),
        {name, _, args_ast} = name_ast,
        name_arity = length(args_ast) do
      {{name, name_arity}, {fun_head_ast, fun_do_ast}}
    end
    |> Enum.into(%{})
  end

end
