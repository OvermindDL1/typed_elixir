defmodule TypedElixir.Type do
  # Oh if only Elixir already had a type system...

  alias TypedElixir.HMEnv


  # Types
  defmodule Const do
    defstruct [:const, :meta]
    def new(env, const, meta \\ []) when is_atom(const) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Const{const: const, meta: meta}
      {env, type}
    end
  end


  defmodule App do
    defstruct [:const, :args_types, :meta]
    def new(env, const, args_types, meta \\ []) when is_atom(const) and is_list(args_types) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %App{const: const, args_types: args_types, meta: meta}
      {env, type}
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
    defstruct [:types, :meta] # TODO:  Figure out what this should have
    def new(env, types, meta \\ []) when is_map(types) and is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Module{types: types, meta: meta}
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
        id = :erlang.now()
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
        id = :erlang.now()
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
      ptr = :erlang.now()
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




  # Resolve a type to a type
  def resolve_types!(env, fromType, intoType) do
    inspect {:RESOLVERINATING, env, fromType, intoType}
    type = resolve_types(env, fromType, intoType)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end


  def resolve_types(env, fromType, intoType) do
    {env, fromType} = get_type_or_ptr_type(env, fromType)
    {env, intoType} = get_type_or_ptr_type(env, intoType)
    resolve_types_nolinks(env, fromType, intoType)
  end


  defp resolve_types_nolinks(env, fromType, intoType)
  defp resolve_types_nolinks(env, type, type), do: {env, type}
  defp resolve_types_nolinks(env, %Const{const: const_type} = type, %Const{const: const_type}) do
    # TODO:  Verify the metas too...
    {env, type}
  end
  defp resolve_types_nolinks(env, fromType, %Ptr.Unbound{}=_intoType), do: {env, fromType} # Everything goes in to an Unbound
  defp resolve_types_nolinks(env, fromType, %Ptr.Generic{named: false}=_intoType), do: {env, fromType} # Everything goes out to an unnamed generic too (good luck recovering it)
  defp resolve_types_nolinks(env, fromType, %Ptr.Generic{named: true}=_intoType), do: {env, fromType} # TODO:  Everything also goes in to a named generic, buuuut.....
  # Keep TypePtr handling last
  # def resolve_types_nolinks(env, %Ptr{ptr: fromPtr}=from, %Ptr{ptr: intoPtr}=into) do
  #   intoType = HMEnv.get_type_ptr(env, intoPtr)
  #   intoPath = resolve_types_nolinks(env, from, intoType)
  #   if Exception.exception?(intoPath) do
  #     fromType = HMEnv.get_type_ptr(env, fromPtr)
  #     resolve_types_nolinks(env, fromType, into)
  #   else
  #     intoPath
  #   end
  # end
  # defp resolve_types_nolinks(env, %Ptr{}=fromType, %Ptr{}=intoType) do
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
  # defp resolve_types_nolinks(env, fromType, %Ptr{}=intoType) do
  #   case Ptr.get(env, intoType) do
  #     %Ptr.Unbound{} -> {env, fromType}
  #     %Ptr.Generic{} -> throw {:CANNOT_RESOLVE_SPECIFIC_TYPE_TO_GENERIC, fromType}
  #   end
  # end
  # defp resolve_types_nolinks(env, fromType, %Ptr{ptr: ptr}) do
  #   # intoType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # defp resolve_types_nolinks(env, %Ptr{ptr: ptr}, intoType) do
  #   # fromType = HMEnv.get_type_ptr(env, ptr)
  #   # resolve_types_nolinks(env, fromType, intoType)
  # end
  # Catch-all
  defp resolve_types_nolinks(env, fromType, intoType) do
    {env, from} = get_type_or_ptr_type(env, fromType)
    {_env, into} = get_type_or_ptr_type(env, intoType)
    throw {:NO_TYPE_RESOLUTION, from, into}
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
