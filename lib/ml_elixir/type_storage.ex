defmodule MLElixir.TypeStorage do
  @moduledoc """
  """

  @ets_table_name :mlelixir_typestorage

  def ets do
    case :global.whereis_name(@ets_table_name) do
      :undefined ->
        {:ok, task} = Task.start(fn -> receive do x -> x end; receive do x -> x end end) # receive ones for table transfer, then again to wait forever
        @ets_table_name = :ets.new(@ets_table_name, [:named_table, :public, :set, read_concurrency: true])
        :ets.give_away(@ets_table_name, task, nil)
        :global.register_name(@ets_table_name, task)
        fill_defaults(@ets_table_name)
        @ets_table_name
      pid when is_pid(pid) -> @ets_table_name
    end
  end


  def fill_defaults(table) do
    define_type(table, Kernel, :+, {})
  end

  def define_type(table \\ ets(), module, name, typedef) do
    :ets.insert_new(table, {{module, name}, typedef})
  end

end
