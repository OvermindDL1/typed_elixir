defmodule TypedElixirTest do
  @moduledoc false

  use ExUnit.Case
  doctest TypedElixir

  test "the truth" do
    assert 1 + 1 == 2
  end


  test "Typed Module" do
    use TypedElixir
    defmodulet TypedTest do
      @moduledoc false

      import String

      @type test_type :: String.t

      @spec simple() :: nil
      def simple(), do: nil

      @spec hello(String.t) :: String.t | Map.t
      def hello(str) when is_binary(str) do
        # @spec ret :: String.t # TODO
        ret = trim(str) <> " world"
        ret
      end

      def fun_no_spec(), do: nil
    end

    assert "Hello world" == TypedTest.hello("Hello")
    # String.blah
  end


end
