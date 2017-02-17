defmodule TypedElixir.HMEnv do
  defstruct counters: %{}, types: %{}, scopes: [], type_ptrs: %{}


  def new_counter(env, key) do
    {value, counters} = Map.get_and_update(env.counters, key, fn
      nil -> {0, 1}
      value -> {value, value+1}
    end)
    env = %{env | counters: counters}
    {env, value}
  end


  # Type helpers

  def push_type(env, key, type) do
    types = Map.update(env.types, key, [type], fn prior -> [type | prior] end)
    env = %{env | types: types}
    env = add_to_scope(env, :type, key)
    env
  end

  def get_type(env, key) do
    case env.types[key] do
      nil -> nil
      [] -> nil
      [type | _] -> type
    end
  end

  defp pop_type(env, key) do
    types = case env.types[key] do
      nil -> throw {:POP_TYPE, key, "does not exist"}
      [] -> throw {:POP_TYPE, key, "does not exist"}
      [_] -> Map.delete(env.types, key)
      [_ | rest] -> Map.put(env.types, key, rest)
    end
    %{env | types: types}
  end


  # Type vars

  def push_type_ptr(env, ptr, type) do
    type_ptrs = Map.update(env.type_ptrs, ptr, [type], fn prior -> [type | prior] end)
    env = %{env | type_ptrs: type_ptrs}
    # env = add_to_scope(env, :type_ptr, ptr) # Unique ID for an entire module, plus might be used elsewhere...
    env
  end

  def get_type_ptr(env, ptr) do
    case env.type_ptrs[ptr] do
      nil -> throw {:GET_TYPE_PTR, ptr, "does not exist", env.type_ptrs}
      [] -> throw {:GET_TYPE_PTR, ptr, "does not exist", env.type_ptrs}
      [type_ptrs | _] -> type_ptrs
    end
  end

  # Never popping type vars because the types may be referenced elsewhere
  # defp pop_type_ptr(env, ptr) do
  #   type_ptrs = case env.type_ptrs[ptr] do
  #     nil -> throw {:POP_type_ptr, ptr, "does not exist"}
  #     [] -> throw {:POP_type_ptr, ptr, "does not exist"}
  #     [_] -> Map.delete(env.type_ptrs, ptr)
  #     [_ | rest] -> Map.put(env.type_ptrs, ptr, rest)
  #   end
  #   %{env | type_ptrs: type_ptrs}
  # end

  def update_type_ptr(env, ptr, type) do
    type_ptrs = Map.update!(env.type_ptrs, ptr, fn [_ | prior] -> [type | prior] end)
    env = %{env | type_ptrs: type_ptrs}
    env
  end


  # Scope helpers

  def push_scope(env, id, doc \\ nil) do
    scopes = [{id, doc, []} | env.scopes]
    %{env | scopes: scopes}
  end

  def pop_scope(env, id) do
    [{^id, _, scope} | scopes] = env.scopes
    env = %{env | scopes: scopes}
    env = Enum.reduce(scope, env, fn ({key, value}, env) -> removed_from_scope(env, key, value) end)
    {env, scope}
  end

  def get_scope(env) do
    [scope | _scopes] = env.scopes
    scope
  end

  defp add_to_scope(env, key, val) do
    [{id, doc, scope} | restScopes] = env.scopes
    scopes = [{id, doc, [{key, val} | scope]} | restScopes]
    %{env | scopes: scopes}
  end

  defp removed_from_scope(env, key, val)
  defp removed_from_scope(env, :type, name), do: pop_type(env, name)
  # defp removed_from_scope(env, :type_ptr, id), do: pop_type_ptr(env, id)


  # Generic helpers

  def map_env(env, enumerable, func), do: map_env(env, enumerable, func, [])

  def map_env(env, [], _func, reversed_results) do
    {env, :lists.reverse(reversed_results)}
  end
  def map_env(env, [value | rest], func, reversed_results) do
    {env, result} = func.(env, value)
    reversed_results = [result | reversed_results]
    map_env(env, rest, func, reversed_results)
  end


  def zipmap_env(env, enumerableLeft, enumerableRight, func) when length(enumerableLeft) === length(enumerableRight) do
    zipmap_env(env, enumerableLeft, enumerableRight, func, [])
  end

  def zipmap_env(env, [], [], _func, reversed_results) do
    {env, :lists.reverse(reversed_results)}
  end
  def zipmap_env(env, [left | restLeft], [right | restRight], func, reversed_results) do
    {env, result} = func.(env, left, right)
    reversed_results = [result | reversed_results]
    zipmap_env(env, restLeft, restRight, func, reversed_results)
  end

end
