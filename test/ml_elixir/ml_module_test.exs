

import MLElixir
defmlmodule MLModuleTest do

  type type_declaration

  type type_definition = integer

  def test_int_untyped = 42
  def test_int_typed | integer = 42
  def test_float_untyped = 6.28
  def test_float_typed | float = 6.28
  def test_defined | type_definition = 42
  def identity_untyped(x) = x
  def identity_typed(x | +id_type) | +id_type = x
  def identity_int(x | integer, _f | float) | integer = x
  def identity_float(_x | integer, f | float) | float = f
  def test_block0 do 42 end
  def test_blockN0() do 42 end
  def test_blockT0() | integer do 42 end
  def test_blockN1(x) do x end
  def test_blockT1(x | +id_type) | +id_type do x end

end


defmlmodule MLModuleTest_Generalized do
  type t

  def blah(t | t) | t, do: t
end
