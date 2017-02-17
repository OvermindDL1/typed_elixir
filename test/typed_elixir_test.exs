defmodule TypedElixirTest do
  @moduledoc false

  use ExUnit.Case
  import CompileTimeAssertions

  doctest TypedElixir
  use TypedElixir


  test "Empty" do
    defmodulet TypedTest_Empty do
    end
  end

  test "Typed - 0-arg" do
    defmodulet TypedTest_Typed_Simple do
      @spec simple() :: nil
      def simple(), do: nil
    end
    assert nil === TypedTest_Typed_Simple.simple()

    assert_compile_time_throw {:NO_TYPE_UNIFICATION, %TypedElixir.TypeConst{type: :atom, meta: %{values: [nil]}}, %TypedElixir.TypeConst{type: :integer, meta: %{values: [1]}}}, fn ->
      use TypedElixir
        defmodulet TypedTest_Typed_Simple do
          @spec simple() :: nil
          def simple(), do: 1
        end
    end
  end


  test "Untyped - 0-arg" do
    defmodulet TypedTest_Untyped_Simple do
      def simple(), do: nil
    end
    assert nil === TypedTest_Untyped_Simple.simple()
  end


  test "Typed - 1-arg - identity" do
    # The extra type is to give a name to the type in simple, so the input and output become the same type.
    # If the spec was `simple(any()) :: any()` then you could not state that the output type is based on the input type.
    defmodulet TypedTest_Typed_Identity do
      @type identity_type :: any()
      @spec identity(identity_type) :: identity_type
      def identity(x), do: x
    end
    assert 42 === TypedTest_Typed_Identity.identity(42)
  end


  test "Untyped - 1-arg - identity" do
    defmodulet TypedTest_Untyped_Identity do
      def identity(x), do: x
    end
    assert 42 === TypedTest_Untyped_Identity.identity(42)
  end


  test "Typed - 1-arg - recursive" do
    defmodulet TypedTest_Typed_Recursive_Counter do
      @spec counter(integer()) :: integer()
      def counter(x), do: counter(x)
    end

    assert_compile_time_throw {:NO_TYPE_RESOLUTION, %TypedElixir.TypeConst{type: :integer, meta: %{}}, %TypedElixir.TypeConst{type: :float, meta: %{values: [6.28]}}}, fn ->
      use TypedElixir
      defmodulet TypedTest_Typed_Recursive_Counter_Bad do
        @spec counter(integer()) :: integer()
        def counter(x), do: counter(6.28)
      end
    end
  end


  test "Typed - 0-arg/1-arg" do
    defmodulet TypedTest_Typed_2func do
      @spec simple() :: nil
      def simple(), do: nil

      @type identity_type :: any()
      @spec identity(identity_type) :: identity_type
      def identity(x), do: x
    end

    assert_compile_time_throw {:NO_TYPE_RESOLUTION, %TypedElixir.TypeConst{type: :integer, meta: %{}}, %TypedElixir.TypeConst{type: :float, meta: %{values: [6.28]}}}, fn ->
      use TypedElixir
      defmodulet TypedTest_Typed_2func do
        @spec simple() :: nil
        def simple(), do: nil

        @type identity_type :: any()
        @spec identity(identity_type) :: identity_type
        def identity(x), do: x
        @spec identity1(identity_type) :: identity_type
        def identity1(x), do: nil
      end
    end
  end


  test "rest" do
    # defmodulet TypedTest1 do
    #   @moduledoc false
    #
    #   @spec simple() :: nil
    #   def simple(), do: nil
    #
    #   @type identity_type :: any()
    #   @spec identity(identity_type) :: identity_type
    #   def identity(x), do: x
    # end
    #
    # defmodulet TypedTest2 do
    #   @moduledoc false
    #
    #   @spec simple(s) :: nil
    #   def simple(s), do: String.capitalize(s)
    # end
    #
    # defmodulet TypedTest3 do
    #   @moduledoc false
    #
    #   alias String, as: S
    #
    #   @spec simple(s) :: nil
    #   def simple(s), do: S.capitalize(s)
    # end
    #
    # defmodulet TypedTest3 do
    #   @moduledoc false
    #
    #   import String
    #
    #   @spec simple(s) :: nil
    #   def simple(s), do: capitalize(s)
    # end

    # defmodulet TypedTest do
    #   @moduledoc false
    #
    #   import String
    #
    #   @type test_type :: String.t
    #
    #   @spec fun_default() :: nil
    #   @spec fun_default(any()) :: nil
    #
    #   @spec simple() :: nil
    #   def simple(), do: nil
    #
    #   @spec hello(test_type) :: test_type | Map.t
    #   def hello(str) when is_binary(str) do
    #     @spec ret :: String.t
    #     ret = str |> trim
    #     ret = ret <> " world"
    #     ret
    #   end
    #   def hello(obj) do
    #     "unknown hello: #{inspect obj}"
    #   end
    #
    #   def fun_default(_t \\ 42), do: nil
    #
    #   def fun_no_spec(_t \\ 42), do: nil
    #
    #   @type pattern :: nil
    #   @opaque t :: nil
    #   @spec replace(t, pattern | Regex.t, t, Keyword.t) :: t
    #   def replace(_,_,_,_), do: nil
    # end

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

    # assert "Hello world" == IO.inspect(TypedTest.hello("Hello"))
  end


end
