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
    defstruct [:meta] # TODO:  Figure out what this should have
    def new(env, meta \\ []) when is_list(meta) do
      meta = Enum.into(meta, %{})
      type = %Module{meta: meta}
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
        {env, id} = HMEnv.new_counter(env, :tval)
        {env, %Unbound{id: id, depth: depth}}
      end
      def new_ptr(env, depth \\ 0) do
        {env, ptrType} = new(env, depth)
        Ptr.new(env, ptrType)
      end
    end

    defmodule Generic do
      defstruct [:id, :named]
      def new(env, named) when is_boolean(named) do
        {env, id} = HMEnv.new_counter(env, :tval)
        {env, %Generic{id: id, named: named}}
      end
      def new_ptr(env, named) do
        {env, ptrType} = new(env, named)
        Ptr.new(env, ptrType)
      end
    end



    def new(env, %Unbound{}=ptype) ,do: new_withtype(env, ptype)
    def new(env, %Generic{}=ptype) ,do: new_withtype(env, ptype)
    def new(env, %Link{}=ptype)    ,do: new_withtype(env, ptype)
    defp new_withtype(env, ptype) do
      {env, ptr} = HMEnv.new_counter(env, :ptr)
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

end
