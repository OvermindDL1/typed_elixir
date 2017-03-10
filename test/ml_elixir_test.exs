defmodule MLElixirTest do
  @moduledoc false

  use ExUnit.Case
  import CompileTimeAssertions

  doctest MLElixir
  import MLElixir


  defmodule NativeTest do

    def identity(x), do: x

    def __ml_open__ do
      %{
        funs: %{
          identity: fn
            (env, meta, [{_,argMeta,_}]=args) ->
              ast = {:"$$CALL$$", [type: argMeta[:type]] ++ meta, [{MLElixirTest.NativeTest, :identity} | args]}
              {env, ast}
            end,
          },
        types: %{
        },
      }
    end

  end

  defmlmodule MLModuleTest_Specific do
    type t = MLModuleTest.type_definition
    type Specific = MLModuleTest_Generalized.(t: float)

    def testering0 | t = 42
  end

  test "MLModuleTest's" do
    assert MLModuleTest.test_int_untyped() === 42
    assert MLModuleTest.test_int_typed() === 42
    assert MLModuleTest.test_float_untyped() === 6.28
    assert MLModuleTest.test_float_typed() === 6.28
    assert MLModuleTest.test_defined() === 42
    assert MLModuleTest.identity_untyped(42) === 42
    assert MLModuleTest.identity_untyped(6.28) === 6.28
    assert MLModuleTest.identity_typed(42) === 42
    assert MLModuleTest.identity_typed(6.28) === 6.28
    assert MLModuleTest.identity_int(42, 6.28) === 42
    assert MLModuleTest.identity_float(42, 6.28) === 6.28
    assert MLModuleTest.test_block0() === 42
    assert MLModuleTest.test_blockN0() === 42
    assert MLModuleTest.test_blockT0() === 42
    assert MLModuleTest.test_blockN1(42) === 42
    assert MLModuleTest.test_blockN1(6.28) === 6.28
    assert MLModuleTest.test_blockT1(42) === 42
    assert MLModuleTest.test_blockT1(6.28) === 6.28

    assert MLModuleTest_Generalized.blah(42) === 42
  end

  # test "literals" do
  #   assert 1 == defml 1
  #   assert 1.2 == defml 1.2
  #   assert :ok == defml :ok
  # end
  #
  # test "let - binding single untyped variable" do
  #   assert 1 == defml let _a = 2 in 1
  #   assert 1 == defml let a = 1 in a
  #   assert 1 == defml let a = 1 in let b = a in b
  # end
  #
  # test "let - named binding single untyped variable" do
  #   assert 1 == defml let _a: _b = 2 in 1
  #   # assert 1 == defml let a: b = 1 in a # These make elixir warn on unused vars, use the below two instead
  #   # assert 1 == defml let a: b = 1 in b # These make elixir warn on unused vars, use the below two instead
  #   assert 1 == defml let a: _b = 1 in a
  #   assert 1 == defml let _a: b = 1 in b
  # end
  #
  # test "let - binding single typed unconstrained variable" do
  #   assert 1 == defml let ![_a: int] = 2 in 1
  #   assert 1 == defml let ![a: int] = 1 in a
  #   assert 1 == defml let a = 1 in let ![b: int] = a in b
  #   assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve mismatched types", fn ->
  #     import MLElixir
  #     defml let ![a: int] = 6.28 in a
  #   end
  # end
  #
  # test "let - binding single typed constrained variable" do
  #   assert 1 == defml let ![a: int a=1] = 1 in a
  #   assert 1 == defml let ![a: int a<=2] = 1 in a
  #   assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
  #     import MLElixir
  #     defml let ![a: int a=2] = 1 in a
  #   end
  #   assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
  #     import MLElixir
  #     defml let ![a: int a>=2] = 1 in a
  #   end
  # end
  #
  # test "let - matching single literal" do
  #   assert 1 == defml let 1 = 1 in 1
  #   assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
  #     import MLElixir
  #     defml let 1 = 2 in 1
  #   end
  # end
  #
  # test "let - multiple" do
  #   assert 1 == defml let a = 1 in let 2 = 2 in a
  #   assert 1 == defml let a = 1 in let b = a in b
  #   assert_compile_time_raise MLElixir.UnificationError, "Unable to resolve", fn ->
  #     import MLElixir
  #     defml let a = 1 in let 2 = a in a
  #   end
  # end
  #
  # test "let - open" do
  #   assert 1 == defml let open MLElixir.Core in 1
  #   assert 3 == defml let open MLElixir.Core in 1 + 2
  #   assert_compile_time_raise MLElixir.InvalidCall, "No such function found", fn ->
  #     import MLElixir
  #     defml no_default_opens: true, do: 1 + 2
  #   end
  #   assert 3 == defml no_default_opens: true, do: let open MLElixir.Core in 1 + 2
  # end
  #
  # test "fun - define" do
  #   # So sorry for this `end`, just an elixir syntax stupidity, not even elixir should have an end with a fn without an explicit block, blah...
  #   # Stupid trailing `end`s, elixir has such an annoying syntax at times...
  #   # defml fn blah -> blah end
  #   # defml fn ![blah: int] -> blah end
  #   # assert 1 == (defml fn -> 1 end).()
  #   # assert 1 == (defml fn x -> x end).(1)
  #   # assert 6.28 == (defml fn x -> x end).(6.28)
  #   # assert 2 == (defml fn ![x: int x>=1] -> x end).(2)
  #   # assert 6.28 == (defml fn ![x: int x>=1] -> x end).(6.28) # When called from 'outside' it is not typed
  # end
  #
  # test "fun - store" do
  #   # quote do
  #   #   fn x -> x
  #   # end
  #   # defml let id = fn x -> x end in id 1
  #   # defml let ![id: int] = fn x -> x end in 1
  #   # assert 1 == defml let id = fn x -> x end in id 1
  # end
  #
  # # test "call" do
  # #   assert 1 == defml 1+2
  # # end


end
