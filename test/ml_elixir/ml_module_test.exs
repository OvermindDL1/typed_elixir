

import MLElixir
defmlmodule MLModuleTest do

  type type_declaration

  type type_definition = integer

  def test_int_untyped = 42
  def test_int_typed | integer = 42
  def test_float_untyped = 6.28
  def test_float_typed | float = 6.28
  def test_defined | type_definition = 42
  def identity_untyped(a) = a
  def identity_typed(b | !id_type) | !id_type = b
  def identity_int(c | integer, f | float) | integer = c
  def identity_float(x | integer, f | float) | float = f
  def test_block0 do 42 end
  def test_blockN0() do 42 end
  def test_blockT0() | integer do 42 end
  def test_blockN1(x) do x end
  def test_blockT1(x | !id_type) | !id_type do x end
  def test_noblockT1(x | !id_type) | !id_type = x

end


defmlmodule MLModuleTest_Generalized do
  type t

  def blah(t | t) | t, do: t
  def bloo(t | t) | t = t
end
