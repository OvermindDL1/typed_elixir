defmodule TypedElixir.Type do
  # Oh if only Elixir already had a static type system...

  alias TypedElixir.HMEnv

  # Yes monotonic is slow, but the ordering is useful for compiling and I do not care since it is not called at run-time
  def unique_value, do: :erlang.unique_integer([:positive, :monotonic])


  # Types
  defmodule Const do
    defstruct [:const, :meta]
    def new(env, const, meta \\ []) when is_atom(const) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Const{const: const, meta: meta}
      {env, type}
    end
  end




  defmodule Ptr do
    defstruct [:ptr]

    defmodule Link do
      defstruct [:type]
      def new(env, type), do: {env, %Link{type: type}}
      def new_ptr(env, type) do
        {env, ptrType} = new(env, type)
        Ptr.new(env, ptrType)
      end
    end

    defmodule Unbound do
      defstruct [:id, :depth]
      def new(env, depth \\ 0) do
        # {env, id} = HMEnv.new_counter(env, :tval)
        id = TypedElixir.Type.unique_value()
        {env, %Unbound{id: id, depth: depth}}
      end
      def new_ptr(env, depth \\ 0) do
        {env, ptrType} = new(env, depth)
        Ptr.new(env, ptrType)
      end
    end

    defmodule Generic do
      defstruct [:id, :named, :meta]
      def new(env, named, meta \\ []) when is_boolean(named) and is_list(meta) do
        # {env, id} = HMEnv.new_counter(env, :tval)
        id = TypedElixir.Type.unique_value()
        {env, %Generic{id: id, named: named, meta: meta}}
      end
      def new_ptr(env, named, meta \\ []) do
        {env, ptrType} = new(env, named, meta)
        Ptr.new(env, ptrType)
      end
    end



    def new(env, %Unbound{}=ptype) ,do: new_withtype(env, ptype)
    def new(env, %Generic{}=ptype) ,do: new_withtype(env, ptype)
    def new(env, %Link{}=ptype)    ,do: new_withtype(env, ptype)
    defp new_withtype(env, ptype) do
      # {env, ptr} = HMEnv.new_counter(env, :ptr)
      ptr = TypedElixir.Type.unique_value()
      type = %Ptr{ptr: ptr}
      env = HMEnv.push_type_ptr(env, ptr, ptype)
      {env, type}
    end

    def get(env, ptr)
    def get(env, %Ptr{ptr: ptr}) do
      HMEnv.get_type_ptr(env, ptr)
    end

    def set(env, ptr, value)
    def set(env, %Ptr{ptr: ptr}, %Unbound{}=value) ,do: HMEnv.update_type_ptr(env, ptr, value)
    def set(env, %Ptr{ptr: ptr}, %Generic{}=value) ,do: HMEnv.update_type_ptr(env, ptr, value)
    def set(env, %Ptr{ptr: ptr}, %Link{}=value)    ,do: HMEnv.update_type_ptr(env, ptr, value)
    def set(env, ptr, to_link) do
      {env, link} = Link.new(env, to_link)
      set(env, ptr, link)
    end

  end




  defmodule App do
    defstruct [:type, :args_types, :meta]
    # args_types is [{:name, type}]
    def new(env, inner_type, args_types, meta \\ []) when is_list(args_types) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %App{type: inner_type, args_types: args_types, meta: meta}
      {env, type}
    end

    def get_type(env, %App{type: type}), do: get_type(env, type)
    def get_type(env, type) do
      {env, resolved} = TypedElixir.Type.get_resolved_type(env, type)
      case resolved do
        %App{} = app -> get_type(env, app)
        t -> {env, t}
      end
    end

    def refine(env, app, overrides), do: refine(env, app, overrides, 0)
    # defp refine(env, %App{}=app, [], _pos_index), do: {env, app}
    defp refine(env, %App{type: type, args_types: args_types} = app, [], _pos_index) do
      Enum.any?(args_types, fn{_, type} ->
        {_env, resolved} = TypedElixir.Type.get_type_or_ptr_type(env, type)
        case resolved do
          %TypedElixir.Type.Ptr.Unbound{} -> true
          _ -> false
        end
      end)
      |> case do
        false -> {env, type} # Fully applied, return the final type
        true -> {env, app} # We are still being applied...
      end
    end
    # By name
    defp refine(env, %App{}=app, [{name, new_type} | overrides], pos_index) when is_atom(name) do
      args_types = app.args_types
      type = app.type
      case List.keyfind(args_types, name, 0, nil) do
        nil -> throw {:TYPE_NAME_NOT_FOUND_ON_APP_TYPE, name, new_type, app}
        {^name, old_type} ->
          {env, ptr} = Ptr.Link.new_ptr(env, new_type)
          args_types = List.keyreplace(args_types, name, 0, {name, ptr})
          {env, type} =
            TypedElixir.Type.map_types(env, type, fn
              (env, ^old_type) -> {env, ptr}
              (env, t) -> {env, t}
            end)
          app = %{app | args_types: args_types, type: type}
          refine(env, app, overrides, pos_index)
      end
    end
    # By position
    defp refine(env, %App{}=app, [new_type | overrides], pos_index) when is_map(new_type) do # is_struct
      args_types = app.args_types
      type = app.type
      case Enum.at(args_types, pos_index, nil) do
        nil -> throw {:TYPE_POST_NOT_FOUND_ON_APP_TYPE, pos_index, new_type, app}
        {name, old_type} ->
          {env, ptr} = Ptr.Link.new_ptr(env, new_type)
          args_types = List.replace_at(args_types, pos_index, {name, ptr})
          {env, type} =
            TypedElixir.Type.map_types(env, type, fn
              (env, ^old_type) -> {env, ptr}
              (env, t) -> {env, t}
            end)
          app = %{app | args_types: args_types, type: type}
          refine(env, app, overrides, pos_index)
      end
    end
  end




  defmodule Func do
    defstruct [:args_types, :return_type, :is_indirect, :meta]
    def new(env, args_types, return_type, is_indirect, meta \\ []) when is_list(args_types) and is_map(return_type) and is_boolean(is_indirect) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Func{args_types: args_types, return_type: return_type, is_indirect: is_indirect, meta: meta}
      {env, type}
    end
  end




  defmodule Module do
    defstruct [:types, :meta]
    def new(env, types, meta \\ []) when is_map(types) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Module{types: types, meta: meta}
      {env, type}
    end
  end




  defmodule GADT do
    # :heads should be [{atom_HeadName, type} || atom_HeadName]
    defstruct [:heads, :meta]
    def new(env, heads, meta \\ []) when is_list(heads) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %GADT{heads: heads, meta: meta}
      {env, type}
    end
  end




  defmodule Record do
    # :labels should be [{atom_Label, type}]
    defstruct [:labels, :meta]
    def new(env, labels, meta \\ []) when is_list(labels) and is_list(meta) do
      labels_no_dups = Enum.uniq_by(labels, &elem(&1, 0))
      if(length(labels_no_dups) != length(labels), do: throw {:RECORD_DUPLICATE_LABEL, labels -- labels_no_dups})
      meta = Enum.into(meta, %{})
      type = %Record{labels: labels, meta: meta}
      {env, type}
    end

    def extend(env, record, new_labels)
    def extend(env, %Record{} = record, []), do: {env, record}
    def extend(env, %Record{labels: labels, meta: meta}, [{nl, _nt} = new_label | rest]) do
      case Enum.find(labels, nil, &(elem(&1, 0)==nl)) do
        nil ->
          new_labels = labels ++ [new_label] # Keep the ordering...
          type = %Record{labels: new_labels, meta: meta}
          extend(env, type, rest)
        label -> throw {:RECORD_EXTENDED_DUPLICATE_LABEL, label}
      end
    end
  end




  # Helpers

  def get_resolved_type(env, type)
  def get_resolved_type(env, %Ptr{}=tvar) do
    case Ptr.get(env, tvar) do
      %Ptr.Link{type: type} -> get_resolved_type(env, type)
      _ -> {env, tvar}
    end
  end
  def get_resolved_type(env, type), do: {env, type}


  def get_type_or_ptr_type(env, type)
  def get_type_or_ptr_type(env, %Ptr{}=tvar) do
    case Ptr.get(env, tvar) do
      %Ptr.Link{type: type} -> get_type_or_ptr_type(env, type)
      ptrType -> {env, ptrType}
    end
  end
  def get_type_or_ptr_type(env, type), do: {env, type}


  # Depth-first pass
  def map_types(env, type, opts \\ [], callback)
  def map_types(env, %Const{} = type, _opts, callback), do: callback.(env, type)
  def map_types(env, %App{type: inner_type} = type, _opts, callback) do
    {env, new_type} = callback.(env, inner_type)
    type = %{type | type: new_type}
    callback.(env, type)
  end
  def map_types(env, %Func{args_types: args_types, return_type: return_type} = type, _opts, callback) do
    {env, args_types} = HMEnv.map_env(env, args_types, callback)
    {env, return_type} = callback.(env, return_type)
    type = %{type | args_types: args_types, return_type: return_type}
    callback.(env, type)
  end
  def map_types(env, %Module{types: types} = type, _opts, callback) do
    {env, types} =
      HMEnv.map_env(env, Enum.into(types, []), fn (env, {name, type}) ->
        {env, t} = callback.(env, type)
        {env, {name, t}}
      end)
    types = Enum.into(types, %{})
    type = %{type | types: types}
    callback.(env, type)
  end
  def map_types(env, %Record{labels: labels} = type, _opts, callback) do
    {env, typed_labels} =
      HMEnv.map_env(env, labels, fn (env, {label, type}) ->
        {env, t} = callback.(env, type)
        {env, {label, t}}
      end)
    type = %{type | labels: typed_labels}
    callback.(env, type)
  end
  def map_types(env, %Ptr{ptr: ptr} = type, opts, callback) do
    ptr =
      if :ptr_recurse in opts do
        throw {:TODO, :map_types_recurse_not_implemented_yet}
      else
        ptr
      end
    type = %{type | ptr: ptr}
    callback.(env, type)
  end
  # def map_types(env, types, opts, callback) when is_list(types) do
  #   throw Enum.map(types, &map_types(&1, callback))
  # end



  def generify_unbound(env, type)
  def generify_unbound(env, types) when is_list(types) do
    HMEnv.map_env(env, types, &generify_unbound/2)
  end
  def generify_unbound(env, %Ptr{} = ptr) do
    case Ptr.get(env, ptr) do
      %Ptr.Unbound{} ->
        {env, generic} = Ptr.Generic.new(env, false)
        env = Ptr.set(env, ptr, generic)
        {env, ptr}
        _ -> {env, ptr}
    end
  end
  def generify_unbound(env, %Func{args_types: args_types}) do
    HMEnv.map_env(env, args_types, &generify_unbound/2)
  end
  def generify_unbound(env, %Const{} = type), do: {env, type}
  def generify_unbound(_env, type) do
    throw {:TODO, :generify_unbound_unhandled, type}
  end




  # Resolve a type to a type
  def resolve_types!(env, fromType, intoType) do
    # inspect {:RESOLVERINATING, env, fromType, intoType}
    type = resolve_types(env, fromType, intoType)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end


  def resolve_types(env, from, into) do
    {env, fromType} = get_type_or_ptr_type(env, from)
    {env, intoType} = get_type_or_ptr_type(env, into)
    resolve_types_nolinks(env, from, fromType, intoType)
  end


  defp resolve_types_nolinks(env, from, fromType, intoType)
  defp resolve_types_nolinks(env, _from, fromType, %Ptr.Unbound{}=_intoType), do: {env, fromType} # Everything goes in to an Unbound
  defp resolve_types_nolinks(env, %Ptr{}=ptr, %Ptr.Unbound{}, intoType) do # Can also refine an unbound too
    env = Ptr.set(env, ptr, intoType)
    {env, ptr}
  end
  defp resolve_types_nolinks(env, _from, type, type), do: {env, type}
  defp resolve_types_nolinks(env, _from, %Const{const: const_type} = type, %Const{const: const_type}) do
    # TODO:  Verify the metas too...
    {env, type}
  end
  defp resolve_types_nolinks(env, _from, fromType, %Ptr.Generic{named: false}=_intoType), do: {env, fromType} # Everything goes out to an unnamed generic too (good luck recovering it)
  defp resolve_types_nolinks(env, _from, fromType, %Ptr.Generic{named: true}=_intoType), do: {env, fromType} # TODO:  Everything also goes in to a named generic, buuuut.....
  # Checking if an applied type matches the other type
  defp resolve_types_nolinks(env, _from, %App{type: type}, %App{type: type}), do: {env, type}
  defp resolve_types_nolinks(env, _from, %App{type: fromType}, intoType) do
    resolve_types(env, fromType, intoType)
    # {env, fromType} = get_type_or_ptr_type(env, fromType)
    # resolve_types_nolinks(env, fromType, intoType)
  end
  defp resolve_types_nolinks(env, _from, fromType, %App{type: intoType}) do
    resolve_types(env, fromType, intoType)
    # {env, intoType} = get_type_or_ptr_type(env, intoType)
    # resolve_types_nolinks(env, fromType, intoType)
  end
  defp resolve_types_nolinks(env, _from, %Record{labels: fromLabels} = type, %Record{labels: toLabels}) do
    fromLabels = Enum.sort(fromLabels)
    toLabels = Enum.sort(toLabels)
    if length(fromLabels) != length(toLabels), do: throw {:NO_TYPE_RESOLUTION, :RECORD_LENGTH_DOES_NOT_MATCH}
    {env, _labels} =
      HMEnv.zipmap_env(env, fromLabels, toLabels, fn
        (env, {label, ltype}, {label, ltype}) -> {env, ltype}
        (env, {label, fromType}, {label, toType}) -> resolve_types(env, fromType, toType)
        # (_env, {label, _}, _) -> throw {:NO_TYPE_RESOLUTION, :RECORD_FROM, label}
        # (_env, _, {label, _}) -> throw {:NO_TYPE_RESOLUTION, :RECORD_TO, label}
      end)
    {env, type}
  end
  # Keep TypePtr handling last
  # def resolve_types_nolinks(env, from, %Ptr{ptr: fromPtr}=from, %Ptr{ptr: intoPtr}=into) do
  #   intoType = HMEnv.get_type_ptr(env, intoPtr)
  #   intoPath = resolve_types_nolinks(env, from, intoType)
  #   if Exception.exception?(intoPath) do
  #     fromType = HMEnv.get_type_ptr(env, fromPtr)
  #     resolve_types_nolinks(env, fromType, into)
  #   else
  #     intoPath
  #   end
  # end
  # defp resolve_types_nolinks(env, from, %Ptr{}=fromType, %Ptr{}=intoType) do
  #   p0 = Ptr.get(env, fromType)
  #   p1 = Ptr.get(env, intoType)
  #   case {p0, p1} do
  #     {p0, %Ptr.Unbound{}} -> {env, p0}
  #     # {%Ptr.Unbound{id: id}, %Ptr.Unbound{id: id}} -> p0
  #     # {%Ptr.Generic{id: id0}, %Ptr.Unbound{id: id1}} -> p0
  #     # {%Ptr.Unbound{id: fromID}, %Ptr.Unbound{id: intoID}} ->
  #     _ -> throw {:TODO_RESOLVE_2_PTRS, p0, p1}
  #   end
  # end
  # defp resolve_types_nolinks(env, from, fromType, %Ptr{}=intoType) do
  #   case Ptr.get(env, intoType) do
  #     %Ptr.Unbound{} -> {env, fromType}
  #     %Ptr.Generic{} -> throw {:CANNOT_RESOLVE_SPECIFIC_TYPE_TO_GENERIC, fromType}
  #   end
  # end
  # defp resolve_types_nolinks(env, from, fromType, %Ptr{ptr: ptr}) do
  #   # intoType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # defp resolve_types_nolinks(env, from, %Ptr{ptr: ptr}, intoType) do
  #   # fromType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # Catch-all
  defp resolve_types_nolinks(_env, from, fromType, intoType) do
    # {env, from} = get_type_or_ptr_type(env, fromType)
    # {_env, into} = get_type_or_ptr_type(env, intoType)
    throw {:NO_TYPE_RESOLUTION, from, fromType, intoType}
  end





  # Unify two types
  def unify_types!(env, t0, t1) do
    type = unify_types(env, t0, t1)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end


  def unify_types(env, t0, t1) do
    {env, t0} = get_type_or_ptr_type(env, t0)
    {env, t1} = get_type_or_ptr_type(env, t1)
    unify_types_nolinks(env, t0, t1)
  end


  def unify_types_nolinks(env, t0, t1)
  def unify_types_nolinks(env, t, t), do: {env, t}
  def unify_types_nolinks(_env, %Ptr.Generic{id: id0, named: true}=t0, %Ptr.Generic{id: id1, named: true}=t1) when id0 !== id1 do
    throw {:NO_TYPE_UNIFICATION, :GENERICS_DO_NOT_MATCH, t0, t1}
  end
  def unify_types_nolinks(_env, t0, %Ptr.Generic{named: false}), do: t0
  def unify_types_nolinks(_env, %Ptr.Generic{named: false}, t1), do: t1
  # Keep TypePtr handling last
  # def unify_types_nolinks(env, %Ptr{}=t0, %Ptr{}=t1) do
  #   type0 = Ptr.get(env, t1)
  #   path0 = unify_types_nolinks(env, t1, type0)
  #   if Exception.exception?(path0) do
  #     type1 = Ptr.get(env, t0)
  #     unify_types_nolinks(env, type1, t0)
  #   else
  #     path0
  #   end
  # end
  # def unify_types_nolinks(env, t0, %Ptr{ptr: ptr}) do
  #   # t1 = HMEnv.get_type_ptr(env, ptr)
  #   # unify_types_nolinks(env, t0, t1)
  # end
  # def unify_types_nolinks(env, %Ptr{ptr: ptr}, t1) do
  #   # t0 = HMEnv.get_type_ptr(env, ptr)
  #   # unify_types_nolinks(env, t0, t1)
  # end
  # Catch-all
  def unify_types_nolinks(env, t0, t1) do
    {env, t0} = get_type_or_ptr_type(env, t0)
    {_env, t1} = get_type_or_ptr_type(env, t1)
    throw {:NO_TYPE_UNIFICATION, :NO_PATH, t0, t1}
  end
end
