ExUnit.start()

defmodule CompileTimeAssertions do
  defmodule DidNotRaise, do: defstruct(message: nil)

  defmacro assert_compile_time_raise(expected_exception, expected_message, fun) do
    actual_exception =
      try do
        Code.eval_quoted(fun)
        %DidNotRaise{}
      rescue
        e -> e
      end

    quote do
      assert unquote(actual_exception.__struct__) === unquote(expected_exception)
      assert unquote(actual_exception.message) === unquote(expected_message)
    end
  end

  defmacro assert_compile_time_throw(expected_error, fun) do
    actual_exception =
      try do
        Code.eval_quoted(fun)
        :DID_NOT_THROW
      catch
        e -> e
      end

    quote do
      assert unquote(expected_error) === unquote(Macro.escape(actual_exception))
    end
  end
end
