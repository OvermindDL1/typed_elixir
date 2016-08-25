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
        ret = trim(str) <> " world"
        ret
      end
      def hello(obj) do
        "unknown hello: #{inspect obj}"
      end

      def fun_default(_t \\ 42), do: nil

      def fun_no_spec(_t \\ 42), do: nil
    end

    assert "Hello world" == TypedTest.hello("Hello")
  end


end
