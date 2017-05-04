defmodule MLElixirTest do
  @moduledoc false

  use ExUnit.Case
  # import CompileTimeAssertions

  doctest MLElixir
  import MLElixir


  defmlmodule MLModuleTest_Specific, debug: [:_resolving, :_module_pretty_output] do
    type t = MLModuleTest.type_definition
    type Specific = MLModuleTest_Generalized.(t: float)
    type st = Specific.t
    type a(t) = t
    type b(c) = c
    type ra = a(integer)
    type rb = b(integer)

    type testering_enum
    | none
    | one
    | integer integer
    | two
    | float float
    | float2 float
    # | t integer # Single value?
    # | t_f0 integer, float # multiple value?
    # | i_f1(integer, float) # Hmm, what for...
    # | t_f2{integer, float} # Hmm... default tuple?
    # | t_f3[i: integer, f: float] # Keyword list version maybe?  Or just make a normal list?
    # | %t_f4{t: integer, f: float} # Map version?
    | tuple_enum0{}
    | tuple_enum1{integer}
    | tuple_enum2{integer, float}
    | tuple_recurception{{integer, float}}
    # | record_enum %{integer: integer}
    # | map_enum %{integer => float}
    # | gadt_enum = (integer | float)

    def testering0 | t = 42
    def testering1 | st = 6.28
    def testering2 | ra = 42
    def testering3 | rb = 42
    def testering4 | a(integer) = 42
    def testering5 | a(float) = 6.28
    def testering6 | testering_enum = none() # Just testing that it works with 0-args too, elixir ast oddness reasons
    def testering7 | testering_enum = one
    def testering8 | testering_enum = two
    def testering9 | testering_enum = integer # Curried!
    def testering9x(x) | testering_enum = integer x # Not-Curried!
    def testering10 | testering_enum = integer 42
    def testering11(x) | testering_enum = integer x
    def testering12 = MLModuleTest.test_int_untyped
    def testering13 = MLModuleTest.test_int_typed
    def testering14 = MLModuleTest.test_record.a.b.c
    def testering15(r) = MLModuleTest.test_record_sub(r).b.c
    # def testering16 = MLModuleTest.testering_enum # Should fail
    def testering17 = MLModuleTest.testering_enum.one
    def testering18 = MLModuleTest.testering_enum.integer # auto-curry gadt head test
    def testering19(i) = MLModuleTest.testering_enum.integer i
    def testering20 = tuple_enum0
    def testering21 = tuple_enum1 42
    def testering22 = tuple_enum2
    def testering23 = tuple_enum2 42
    def testering24(f) = tuple_enum2 42, f
    def testering25 = tuple_enum2 42, 6.28
    def testering26 = testering0
    def testering27 = tuple_recurception {42, 6.28}

    # Records (I.E. Erlang/Elixir atom() keyed maps, like structs)
    type record0 = %{} # Empty record
    type record1 = %{
      x: integer,
      y: float,
    }
    type record2(t) = %{t: t}
    type record_ex_0 = %{+: record0, z: integer}
    type record_ex_1 = %{+: record1, +: record0, z: integer}
    type record_ex_2(t) = %{+: record2(t), z: integer}
    type record_ex_2_float = %{+: record2(float), z: integer}
    # type record_ex_sub(t) = %{+: t, z: integer} # Need to support unbound's perhaps? # Unsure if I want to support this...
    type record_rem_0 = %{+: record1, -: x}
    type record_emb_0 = %{a: %{b: %{c: integer}}}

    def testering_record0 | record0 = %{}
    def testering_record1 | record1 = %{x: 42, y: 6.28}
    def testering_record2(i) | record1 = %{x: i, y: 6.28}
    def testering_record3(t | !t) | record2(!t) = %{t: t}
    def testering_record4 | record_ex_0 = %{z: 42}
    def testering_record5 | record_ex_1 = %{x: 42, y: 6.28, z: 42}
    def testering_record6 | record_ex_2(integer) = %{t: 42, z: 42}
    def testering_record7(t | !t) | record_ex_2(!t) = %{t: t, z: 42}
    def testering_record8 | record_ex_2_float = %{t: 6.28, z: 42}
    # def testering_record9 | record_ex_sub(record2(float)) = %{t: 6.28, z: 42} # Unsure if I want to support this...
    def testering_record10 | record_rem_0 = %{y: 6.28}
    def testering_record11(r | record_emb_0) = r.a.b.c

    # Tuples
    type tuple0 = {}
    type tuple1 = {integer}
    type tuple2 = {integer, float}
    type tuple3 = {integer, {float, integer}}

    def testering_tuple0 | tuple0 = {}
    def testering_tuple1(t) | tuple0 = t
    def testering_tuple2 = {}
    def testering_tuple3 = {42}
    def testering_tuple4 | tuple1 = {42}
    def testering_tuple5(i) | tuple1 = {i}
    def testering_tuple6(t) | tuple1 = t
    def testering_tuple7 | tuple1 = testering_tuple3
    def testering_tuple8 = {42, 6.28}
    def testering_tuple9(i|!t) = {i, 6.28}
    def testering_tuple10(i) | tuple2 = testering_tuple9 i
    def testering_tuple11 | tuple2 = testering_tuple9 42
    def testering_tuple12 | tuple3 = {42, {6.28, 42}}

    # FFI
    external addi(integer, integer) | integer = Kernel.+
    def testering_ffi_addi_0 = addi(1, 2)
    def testering_ffi_addi_1(i) = addi(1, i)
    def testering_ffi_addi_2(a, b) = addi(a, b)
  end

  # defmlmodule MLModuleTest_Specific_Module | MLModuleTest.(t: int) do
  # end

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
    assert MLModuleTest_Generalized.bloo(42) === 42

    assert MLModuleTest_Specific.testering0() === 42
    assert MLModuleTest_Specific.testering1() === 6.28
    assert MLModuleTest_Specific.testering2() === 42
    assert MLModuleTest_Specific.testering3() === 42
    assert MLModuleTest_Specific.testering4() === 42
    assert MLModuleTest_Specific.testering5() === 6.28
    assert MLModuleTest_Specific.testering6() === :none
    assert MLModuleTest_Specific.testering7() === :one
    assert MLModuleTest_Specific.testering8() === :two
    assert MLModuleTest_Specific.testering9().(42) === {:integer, 42} # Ooo, can auto-curry enum heads!  Guarded too
    assert MLModuleTest_Specific.testering9x(42) === {:integer, 42}
    assert MLModuleTest_Specific.testering10() === {:integer, 42}
    assert MLModuleTest_Specific.testering11(42) === {:integer, 42}
    assert MLModuleTest_Specific.testering12 === 42
    assert MLModuleTest_Specific.testering13 === 42
    assert MLModuleTest_Specific.testering14 === 42
    assert MLModuleTest_Specific.testering15(%{a: %{b: %{c: 42}}}) === 42
    # assert MLModuleTest_Specific.testering16 === 42 # Should fail
    assert MLModuleTest_Specific.testering17 === :one
    assert MLModuleTest_Specific.testering18.(42) === {:integer, 42}
    assert MLModuleTest_Specific.testering19(42) === {:integer, 42}
    assert MLModuleTest_Specific.testering20 === {:tuple_enum0}
    assert MLModuleTest_Specific.testering21 === {:tuple_enum1, 42}
    assert MLModuleTest_Specific.testering22.(42, 6.28) === {:tuple_enum2, 42, 6.28}
    assert MLModuleTest_Specific.testering23.(6.28) === {:tuple_enum2, 42, 6.28}
    assert MLModuleTest_Specific.testering24(6.28) === {:tuple_enum2, 42, 6.28}
    assert MLModuleTest_Specific.testering25 === {:tuple_enum2, 42, 6.28}
    assert MLModuleTest_Specific.testering26 === 42
    assert MLModuleTest_Specific.testering27 === {:tuple_recurception, {42, 6.28}}

    # Records
    assert MLModuleTest_Specific.testering_record0() === %{}
    assert MLModuleTest_Specific.testering_record1() === %{x: 42, y: 6.28}
    assert MLModuleTest_Specific.testering_record2(42) === %{x: 42, y: 6.28}
    assert MLModuleTest_Specific.testering_record3(42) === %{t: 42}
    assert MLModuleTest_Specific.testering_record4() === %{z: 42}
    assert MLModuleTest_Specific.testering_record5() === %{x: 42, y: 6.28, z: 42}
    assert MLModuleTest_Specific.testering_record6() === %{t: 42, z: 42}
    assert MLModuleTest_Specific.testering_record7(42) === %{t: 42, z: 42}
    assert MLModuleTest_Specific.testering_record8() === %{t: 6.28, z: 42}
    assert MLModuleTest_Specific.testering_record10() === %{y: 6.28}
    assert MLModuleTest_Specific.testering_record11(%{a: %{b: %{c: 42}}}) === 42

    # Tuples
    assert MLModuleTest_Specific.testering_tuple0() === {}
    assert MLModuleTest_Specific.testering_tuple1({}) === {}
    assert MLModuleTest_Specific.testering_tuple2() === {}
    assert MLModuleTest_Specific.testering_tuple3() === {42}
    assert MLModuleTest_Specific.testering_tuple4() === {42}
    assert MLModuleTest_Specific.testering_tuple5(42) === {42}
    assert MLModuleTest_Specific.testering_tuple6({42}) === {42}
    assert MLModuleTest_Specific.testering_tuple7() === {42}
    assert MLModuleTest_Specific.testering_tuple8() === {42, 6.28}
    assert MLModuleTest_Specific.testering_tuple9(42) === {42, 6.28}
    assert MLModuleTest_Specific.testering_tuple10(42) === {42, 6.28}
    assert MLModuleTest_Specific.testering_tuple11() === {42, 6.28}
    assert MLModuleTest_Specific.testering_tuple12() === {42, {6.28, 42}}

    # FFI
    assert MLModuleTest_Specific.addi(1, 2) === 3 # Yep, the external is exposed directly as a function for non-typed code
    assert MLModuleTest_Specific.testering_ffi_addi_0() === 3
    assert MLModuleTest_Specific.testering_ffi_addi_1(3) === 4
    assert MLModuleTest_Specific.testering_ffi_addi_2(1, 2) === 3
  end



  test "MLElixir Javascript output" do
    defmlmodule :testing_js, output_format: :js do
      type t = MLModuleTest.type_definition
      type Specific = MLModuleTest_Generalized.(t: float)
      type st = Specific.t
      type a(t) = t
      type b(c) = c
      type ra = a(integer)
      type rb = b(integer)

      type testering_enum
      | none
      | one
      | integer integer
      | two
      | float float
      | float2 float

      def testering0 | t = 42
      def testering1 | st = 6.28
      def testering2 | ra = 42
      def testering3 | rb = 42
      def testering4 | a(integer) = 42
      def testering5 | a(float) = 6.28
      def testering6 | testering_enum = none() # Just testing that it works with 0-args too, elixir ast oddness reasons
      def testering7 | testering_enum = one
      def testering8 | testering_enum = two
      def testering9 | testering_enum = integer # Curried!
      def testering9x(x) | testering_enum = integer x # Not-Curried!
      def testering10 | testering_enum = integer 42
      def testering11(x) | testering_enum = integer x

      type record0 = %{}
      type record1 = %{
        x: integer,
        y: float,
      }
      type record2(t) = %{t: t}
      type record_ex_0 = %{+: record0, z: integer}
      type record_ex_1 = %{+: record1, +: record0, z: integer}
      type record_ex_2(t) = %{+: record2(t), z: integer}
      type record_ex_2_float = %{+: record2(float), z: integer}
      type record_rem_0 = %{+: record1, -: x}

      def testering_record0 | record0 = %{}
      def testering_record1 | record1 = %{x: 42, y: 6.28}
      def testering_record2(i) | record1 = %{x: i, y: 6.28}
      def testering_record3(t | !t) | record2(!t) = %{t: t}
      def testering_record4 | record_ex_0 = %{z: 42}
      def testering_record5 | record_ex_1 = %{x: 42, y: 6.28, z: 42}
      def testering_record6 | record_ex_2(integer) = %{t: 42, z: 42}
      def testering_record7(t | !t) | record_ex_2(!t) = %{t: t, z: 42}
      def testering_record8 | record_ex_2_float = %{t: 6.28, z: 42}
      def testering_record10 | record_rem_0 = %{y: 6.28}

      # FFI
      external addi(integer, integer) | integer = Kernel.add
      def testering_ffi_addi_0 = addi(1, 2)
      def testering_ffi_addi_1(i) = addi(1, i)
      def testering_ffi_addi_2(a, b) = addi(a, b)
    end
    # |> (fn js ->
    #   IO.puts("Javascript:")
    #   IO.puts(js)
    # end).()
  end



  test "MLElixir macro calls" do
    defmlmodule MLElixir.Tests.MacroTest do
      # def&macro test_macro0 = 42
      # let&macro test_macro0 | MLElixir.MLMacro.ast = 42
      # def&macro test_macro0 | MLElixir.MLMacro.ast, do: MLElixir.MLMacro.ast.one
      # let&macro test_macro0, do: 42
    end
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
