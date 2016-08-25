defmodule TypedElixirTest do
  @moduledoc false

  use ExUnit.Case
  doctest TypedElixir


  test "Typed Module" do
    use TypedElixir
    defmodulet TypedTest do
      @moduledoc false

      import String

      @type test_type :: String.t

      @spec fun_default() :: nil
      @spec fun_default(any()) :: nil

      @spec simple() :: nil
      def simple(), do: nil

      @spec hello(test_type) :: test_type | Map.t
      def hello(str) when is_binary(str) do
        @spec ret :: String.t
        ret = str |> trim
        ret = ret <> " world"
        ret
      end
      def hello(obj) do
        "unknown hello: #{inspect obj}"
      end

      def fun_default(_t \\ 42), do: nil

      def fun_no_spec(_t \\ 42), do: nil

      @type pattern :: nil
      @opaque t :: nil
      @spec replace(t, pattern | Regex.t, t, Keyword.t) :: t
      def replace(_,_,_,_), do: nil
    end

    # IO.inspect TypedElixir.get_module_types_funspecs(String)

    # [{_modulename, objbin}] = Code.compile_quoted(quote do
    #   defmodule Testering do
    #     @type pattern :: nil
    #     @opaque t :: nil
    #     @spec replace(t, pattern | Regex.t, t, Keyword.t) :: t
    #     def replace(_,_,_,_), do: nil
    #   end
    # end)
    # {:ok, {_modname, [abstract_code: abscode]}} = :beam_lib.chunks(objbin, [:abstract_code])
    # {:raw_abstract_v1, code} = abscode
    # code
    # |> Enum.reduce(%{opaques: [], types: [], specs: []}, fn
    #   {:attribute, _line, :spec, {raw_fun, [raw_first_clause | _rest]} = newspec}, %{specs: spec} = acc ->
    #     # IO.inspect {"Found spec", raw_fun, raw_first_clause, _rest}
    #     %{acc | specs: [newspec|spec]}
    #   {:attribute, _line, :type, {name, type_form, var_form} = newtype}, %{types: type} = acc ->
    #     # IO.inspect {"Found type", name, type_form, var_form}
    #     %{acc | types: [newtype|type]}
    #   {:attribute, _line, :opaque, {name, type_form, var_form} = newopaque}, %{opaques: opaque} = acc ->
    #     # IO.inspect {"Found opaque", name, type_form, var_form}
    #     %{acc | opaques: [newopaque|opaque]}
    #   _, acc -> acc
    # end)
    # # |> IO.inspect

    assert "Hello world" == IO.inspect(TypedTest.hello("Hello"))
  end


end
