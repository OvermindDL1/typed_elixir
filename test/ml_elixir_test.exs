defmodule MLElixirTest do
  @moduledoc false

  use ExUnit.Case
  import CompileTimeAssertions

  doctest MLElixir
  import MLElixir


  test "literals" do
    assert 1 == defml 1
    assert 1.2 == defml 1.2
    assert :ok == defml :ok
  end

  test "let - binding single untyped variable" do
    assert 1 == defml let _a = 2 in 1
    assert 1 == defml let a = 1 in a
  end

  test "let - named binding single untyped variable" do
    assert 1 == defml let _a: _b = 2 in 1
    # assert 1 == defml let a: b = 1 in a # These make elixir warn on unused vars, use the below two instead
    # assert 1 == defml let a: b = 1 in b # These make elixir warn on unused vars, use the below two instead
    assert 1 == defml let a: _b = 1 in a
    assert 1 == defml let _a: b = 1 in b
  end

  test "let - binding single typed unconstrained variable" do
    assert 1 == defml let ![_a: int] = 2 in 1
    assert 1 == defml let ![a: int] = 1 in a
    assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve mismatched types", fn ->
      import MLElixir
      defml let ![a: int] = 6.28 in a
    end
  end

  test "let - binding single typed constrained variable" do
    assert 1 == defml let ![a: int a=1] = 1 in a
    assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
      import MLElixir
      defml let ![a: int a=2] = 1 in a
    end
  end

  test "let - matching single literal" do
    assert 1 == defml let 1 = 1 in 1
    assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
      import MLElixir
      defml let 1 = 2 in 1
    end
  end

  test "let - multiple" do
    assert 1 == defml let a = 1 in let 2 = 2 in a
    assert 1 == defml let a = 1 in let b = a in b
    assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
      import MLElixir
      defml let a = 1 in let 2 = a in a
    end
  end


end
