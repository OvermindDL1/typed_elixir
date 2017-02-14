defmodule MLElixir do
  @moduledoc """
  Entirely new syntax within the elixir parser
  """

  # import MLElixir, only: [defml: 1]; defml 42
  # import MLElixir, only: [defml: 1]; defml let a = 42 in a
  # import MLElixir, only: [defml: 1]; defml 1+2
  # import MLElixir, only: [defml: 1]; defml let a = 1 in a+2
  # import MLElixir, only: [defml: 1]; defml module TestML: [id = fn(x)->x end]
  # import MLElixir, only: [defml: 1]; defml let id x = x in module TestML: [x = x, y]


  defmodule MLEnv do
    defstruct counter: -1, type_funs: %{}, type_bindings: %{}, type_vars: %{}
  end

  defp increment_counter(env) do
    %{env | counter: env.counter+1}
  end

  # Exceptions

  defmodule UnknownCall do
    defexception env: %MLEnv{}, name: "<unknown>", args: [], meta: nil
    def message(e) do
      args = Enum.map(e.args, fn({_, meta, _}) -> elem(meta[:type], 0) end)
      line = if e.meta === nil, do: -1, else: e.meta[:line] || -1
      case e.env.type_funs[e.name] do
        nil -> "#{line}:Unknown Call of `#{e.name}` with args of #{inspect args} because the function is not known"
        heads ->
          choices = Enum.map(heads, fn(type) -> elem(type, 0) end)
          "#{line}:Unknown Call of `#{e.name}` with args of #{inspect args} in: #{inspect choices}"
      end
    end
  end

  defmodule UnknownVar do
    defexception env: %MLEnv{}, name: "<unknown>", scope: [], meta: nil
    def message(e) do
      line = if e.meta === nil, do: -1, else: e.meta[:line] || -1
      "#{line}:Access of unknown var `#{e.name}`"
    end
  end

  defmodule UnificationError do
    defexception env: %MLEnv{}, type0: nil, type1: nil
    def message(e) do
      "Unification error between `#{inspect e.type0}` and `#{inspect e.type1}`"
    end
  end




  # Types
  @type_const_ :"$$TCONST$$" # {@type_const, atom,                  meta}
  @type_app   :"$$TAPP$$"   # {@type_app,   type,          [type], meta}
  @type_func  :"$$TFUNC$$"  # {@type_func,  [type],         type,  meta}
  @type_var   :"$$TVAR$$"   # {@type_var,   env.type_vars,         meta}

  # env.type_vars
  @type_var_unbound :"$$TVARUNBOUND$$" # {@type_var_unbound, string, int, meta}
  @type_var_link    :"$$TVARLINK$$"    # {@type_var_link,    type,        meta}
  @type_var_generic :"$$TVARGENERIC$$" # {@type_var_generic, string,      meta}

  @tag_lit      :"$$LIT$$"
  @tag_var      :"$$VAR$$"
  @tag_let      :"$$LET$$"
  @tag_call     :"$$CALL$$"

  @type_const   :"$$TCONST$$"
  @type_generic :"$$TGENERIC$$"
  @type_ptr     :"$$TPTR$$"


  defmacro defml(expr) do
    base_ml_env = %MLEnv{
      type_funs: %{
        :+ =>
          [
            { [{:int, []}, {:int, []}], fn _ -> {:int, []} end },
            { [{:float, []}, {:float, []}], fn _ -> {:float, []} end },
          ],
        },
    }
    IO.inspect {:DEFML1, expr}
    {ml_env, ml_ast} = parse_ml_expr(base_ml_env, expr)
    IO.inspect {:MLAST, ml_ast}
    # {inferred_env, inferred_ast} = infer_ml_ast(ml_env, ml_ast)
    # IO.inspect {:MLINFERREDAST, inferred_ast}
    reify_ml_expr(ml_env, ml_ast)
  end


  defp parse_ml_expr(env, expr)

  # Literals... why does Elixir not give line information for these?!?
  defp parse_ml_expr(env, int)   when is_integer(int), do: {env, {@tag_lit, [type: {@type_const, :int,   [value: int]}],   int}}
  defp parse_ml_expr(env, float) when is_float(float), do: {env, {@tag_lit, [type: {@type_const, :float, [value: float]}], float}}
  defp parse_ml_expr(env, atom)  when is_atom(atom),   do: {env, {@tag_lit, [type: {@type_const, :atom,  [value: atom]}],   atom}}

  defp parse_ml_expr(env, {:module, module_meta, [[{module_name, module_defs}]]}) when is_atom(module_name) do
    defs = Enum.map(module_defs, &parse_module_def(&1, env))
    IO.inspect {:MODULE, module_name, defs}
    :todo
  end

  # TODO:  Add `rec`/`and` handling here

  # Let variable binding to expression with following
  defp parse_ml_expr(env, {:let, let_meta, [{:=, equal_meta, [binding_ast, {:in, in_meta, [inner_expr_ast, after_expr_ast]}]}]}) do
    {envBinding, binding} = parse_binding(env, binding_ast)
    {envExprInner, exprInner} = parse_ml_expr(envBinding, inner_expr_ast)
    envInnerResolved = resolve_binding(envExprInner, binding, exprInner)
    {envExprAfter, exprAfter} = parse_ml_expr(envInnerResolved, after_expr_ast)
    ast = {@tag_let, [type: type_of_expr(envExprAfter, exprAfter)] ++ let_meta, [binding, exprInner, exprAfter]}
    {envExprAfter, ast}
  end

  # # Let variable binding to expression without following
  # defp parse_ml_expr(env, {:let, let_meta, [{:=, equal_meta, [binding_ast, expr_ast]}]}) do
  #   {envInner, binding} = parse_binding(binding_ast)
  # end

  # A variable lookup
  defp parse_ml_expr(env, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
    var_type = type_var(env, name, scope)
    ast = {@tag_var, [type: var_type] ++ meta, [name, scope]}
    {env, ast}
  end

  # A function call
  defp parse_ml_expr(env, {name, call_meta, call_args}) when is_atom(name) and is_list(call_args) do
    args = Enum.map(call_args, &elem(parse_ml_expr(env, &1), 1))
    type = type_call(env, name, args)
    ast = {@tag_call, [type: type] ++ call_meta, [name | args]}
    {env, ast}
  end


  defp parse_module_def({:=, equal_meta, [name_ast, expr]}, env) do
    :blah
  end


  # Typeless variable binding
  defp parse_binding(env, {name, meta, scope}) when is_atom(name) and not is_list(scope) do
    env = increment_counter(env)
    ptr_type = {@type_generic, nil, []}
    type = {@type_ptr, env.counter, []}
    newEnv = %{env |
      type_bindings: Map.put(env.type_bindings, name, type),
      type_vars: Map.put(env.type_vars, env.counter, ptr_type)
      }
    ast = {@tag_var, [type: type] ++ meta, [name, scope]}
    {newEnv, ast}
  end


  # The binding is but a single variable, resolve it
  defp resolve_binding(env, {@tag_var, bindMeta, [name, scope]}, {_, exprMeta, _}) do
    bindType = bindMeta[:type]
    exprType = exprMeta[:type]
    case bindType do
      {@type_ptr, ptr, _typePtrMeta} ->
        case env.type_vars[ptr] do
          nil -> raise %UnificationError{type0: bindType}
          t ->
            tightestType = unify_types!(env, t, exprType)
            %{env |
              type_vars: Map.put(env.type_vars, ptr, tightestType)
            }
        end
      t -> raise %UnificationError{type0: t, type1: exprType}
    end
  end



  # Inferring

  # defp new_var(env, depth, meta \\ [], vmeta \\ []) do
  #   env = increment_counter(env)
  #   env = %{env |
  #     type_vars: Map.put_new(env.type_vars, env.counter, {@type_var_unbound, env.counter, depth, vmeta})
  #   }
  #   {env, {@type_var, env.counter, meta}}
  # end
  #
  # defp new_var_generic(env, meta \\ [], vmeta \\ []) do
  #   env = increment_counter(env)
  #   env = %{env |
  #     type_vars: Map.put_new(env.type_vars, env.counter, {@type_var_generic, env.counter, vmeta})
  #   }
  #   {env, {@type_var, env.counter, meta}}
  # end
  #
  #
  # defp unify_check_depth(env, _id, _int, {@type_const, _name, _meta}), do: env
  # defp unify_check_depth(env, id, int, {@type_app, type, type_args, _meta}) do
  #   env = Enum.reduce(type_args, env, fn(t, e) -> unify_check_depth(e, id, int, t) end)
  #   unify_check_depth(env, type)
  # end
  # defp unify_check_depth(env, id, int, {@type_func, type_args, type, _meta}) do
  #   env = unify_check_depth(env, type)
  #   Enum.reduce(type_args, env, fn(t, e) -> unify_check_depth(e, id, int, t) end)
  # end
  # defp unify_check_depth(env, id, int, {@type_var, vid, _meta}=t) do
  #   case env.type_vars[vid] do
  #     {@type_var_link, vvtype, _vvmeta} -> unify_check_depth(env, id, int, vvtype)
  #     {@type_var_generic, _vvid, _vvmeta} -> raise %UnificationError{type0: t}
  #     {@type_var_unbound, ^id, _vvint, _vvmeta} -> raise %UnificationError{type0: t}
  #     {@type_var_unbound, vvid, vvint, vvmeta} when vvint>int ->
  #       %{env | type_vars: Map.put(env.type_vars, vvid, {@type_var_unbound, vvid, int, vvmeta})}
  #     {@type_var_unbound, vvid, _vvint, _vvmeta} -> env
  #   end
  # end
  #
  #
  # defp unify_type_list(_env, [], t), do: raise %UnificationError{type0: nil, type1: t}
  # defp unify_type_list(_env, t, []), do: raise %UnificationError{type0: t, type1: nil}
  # defp unify_type_list(env, [t0|rest0], [t1|rest1]) do
  #   env = unify(env, t0, t1)
  #   unify_type_list(env, rest0, rest1)
  # end
  #
  #
  # defp unify(env, type0, type1)
  # defp unify(env, type0, type0), do: env
  # defp unify(env, {@type_const, name, _meta0}, {@type_const, name, _meta1}), do: env
  # defp unify(env, {@type_app, type0, type_args0, _meta0}, {@type_app, type1, type_args1, _meta1}) do
  #   env = unify(env, type0, type1)
  #   unify_type_list(env, type_args0, type_args1)
  # end
  # defp unify(env, {@type_func, type_args0, type0, _meta0}, {@type_func, type_args1, type1, _meta1}) do
  #   env = unify_type_list(env, type_args0, type_args1)
  #   unify(env, type0, type1)
  # end
  # defp unify(env, {@type_var, id0, _meta0}, {@type_var, id1, _meta1}) do
  #   v0 = env.type_vars[id0]
  #   v1 = env.type_vars[id1]
  #   case {v0, v1} do
  #     {{@type_var_link, type0, _meta0}, type1} -> unify(type0, type1)
  #     {type0, {@type_var_link, type1, _meta1}} -> unify(type0, type1)
  #     {{@type_var_unbound, id, _int0, _meta0}=t0, {@type_var_unbound, id, _int1, _meta1}=t1} -> raise %UnificationError{type0: t0, type1: t1}
  #     {{@type_var_unbound, id, int, meta}, t} ->
  #       env = unify_check_depth(env, id, int, t)
  #       %{env | type_vars: Map.put(env.type_vars, id, {@type_var_link, type, meta})}
  #     {t, {@type_var_unbound, id, int, meta}} ->
  #       env = unify_check_depth(env, id, int, t)
  #       %{env | type_vars: Map.put(env.type_vars, id, {@type_var_link, type, meta})}
  #     {t0, t1} -> raise %UnificationError{type0: t0, type1: t1}
  #   end
  # end


  # defp generalize(env, depth, type)
  # defp generalize(env, depth, {@type_const, _name, _meta0} = type), do: {env, type}
  # defp generalize(env, depth, {@type_app, type, type_args, meta}) do
  #   {env, type} = generalize(env, depth, type)
  #   {env, type_args} = Enum.reduce(type_args, {env, []}, fn(ty, {en, sf}) ->
  #     {e, t} = generalize(en, depth, ty)
  #     {e, sf++[t]}
  #   end)
  #   {env, {@type_app, type, type_args, meta}}
  # end
  # defp generalize(env, depth, {@type_func, type_args, type, meta}) do
  #   {env, type_args} = Enum.reduce(type_args, {env, []}, fn(ty, {en, sf}) ->
  #     {e, t} = generalize(en, depth, ty)
  #     {e, sf++[t]}
  #   end)
  #   {env, type} = generalize(env, depth, type)
  #   {env, {@type_func, type_args, type, meta}}
  # end
  # defp generalize(env, depth, {@type_var, id, meta}=t) do
  #   case env.type_var[id] do
  #     {@type_var_unbound, vid, vdepth, vmeta} when vdepth > depth ->
  #       env = %{env | type_vars: Map.put(env.type_vars, id, {@type_var_generic, vid, vmeta})}
  #       {env, t}
  #     {@type_var_link, type, meta} ->
  #       %{env | type_vars: Map.put(env.type_vars, id, generalize(env, depth, type))}
  #     _ -> {env, t}
  #   end
  # end
  #
  #
  # defp instantiate(env, depth, type) do
  #   {env, _varmap, type} = instantiate(env, %{}, depth, type)
  #   {env, type}
  # end
  # defp instantiate(env, varmap, _depth, {@type_const, _name, _meta0} = type), do: {env, varmap, type}
  # defp instantiate(env, varmap, depth, {@type_app, type, type_args, meta}) do
  #   {env, varmap, type} = instantiate(env, varmap, depth, type)
  #   {env, varmap, type_args} = Enum.reduce(type_args, {env, varmap, []}, fn(ty, {en, vm, sf}) ->
  #     {e, v, t} = instantiate(en, vm, depth, ty)
  #     {e, v, sf++[t]}
  #   end)
  #   {env, varmap, {@type_app, type, type_args, meta}}
  # end
  # defp instantiate(env, varmap, depth, {@type_func, type_args, type, meta}) do
  # {env, varmap, type} = instantiate(env, varmap, depth, type)
  #   {env, varmap, type_args} = Enum.reduce(type_args, {env, varmap, []}, fn(ty, {en, vm, sf}) ->
  #     {e, v, t} = instantiate(en, vm, depth, ty)
  #     {e, v, sf++[t]}
  #   end)
  #   {env, varmap, {@type_app, type_args, type, meta}}
  # end
  # defp instantiate(env, varmap, depth, {@type_var, id, meta}=t) do
  #   case env.type_var[id] do
  #     {@type_var_unbound, vid, vdepth, vmeta} -> {env, varmap, t}
  #     {@type_var_link, type, meta} -> instantiate(env, varmap, depth, type)
  #     {@type_var_generic, vid, vmeta} ->
  #       case varmap[vid] do
  #         nil ->
  #           {env, var} = new_var(env, depth)
  #           varmap = Map.put(varmap, id, var)
  #           {env, varmap, var}
  #         var -> {env, varmap, var}
  #       end
  #   end
  # end
  #
  #
  # defp verify_fun_type(env, num_params, type)
  # defp verify_fun_type(env, num_params, {@type_func, type_args, type, meta}=t) do
  #   if length(type_args) != num_params do
  #     raise %UnificationError{type0: t}
  #   else
  #     {env, type_args, type}
  #   end
  # end
  # defp verify_fun_type(env, num_params, {@type_var, id, meta}=t) do
  #   case env.type_vars[id] do
  #     {@type_var_link, type, _meta} -> verify_fun_type(env, num_params, type)
  #     {@type_var_unbound, id, depth, meta} ->
  #       {env, args} = Enum.reduce(0..num_params, {env, []}, fn(i, {env, acc}) ->
  #         {env, var} =  new_var(env, i)
  #         {env, acc ++ var}
  #       end)
  #       {env, return} = new_var(env, depth)
  #       %{env | type_vars: Map.put(env.type_vars, id, {@type_var_link, {@type_func, args, return, meta}, meta})}
  #       {env, args, return}
  #   end
  # end
  # defp verify_fun_type(env, num_params, t), do: raise %UnificationError{type0: t}
  #
  #
  # defp infer_ml_ast(env, depth \\ 0, ast)
  #
  # # Literals always have known types
  # defp infer_ml_ast(env, depth, {@tag_lit, _meta, _value} = ast), do: {env, ast}
  #
  # defp infer_ml_ast(env, depth, {@tag_var, meta, [_name, _scope]} = ast) do
  #   try do
  #     {env, type} = instantiate(env, depth, meta.type)
  #     {env, ast}
  #   catch
  #     _,_ -> raise %UnificationError{type0: meta.type}
  #   end
  # end
  #
  # defp infer_ml_ast(env, depth, {@tag_call, call_meta, [call_name | call_args]} = ast) do
  #   {env, fn_ast} = infer_ml_ast(env, depth, )
  #   {env, args, return} = verify_fun_type(env, length(call_args), )
  # end



  # Typing

  defp type_is_fullfilled_by({type, type_meta}, {constraint_type, constraint_meta}) do
    # TODO:  Compare the meta's as necessary too perhaps?
    case {type, constraint_type} do
      {type, type} -> true
      _ -> false
    end
  end

  defp type_of_expr(_env, {_, meta, _}), do: meta[:type]

  defp type_call_does_match?([], []), do: true
  defp type_call_does_match?([doesThis | doesRest], [intoThis | intoRest]) do
    if type_is_fullfilled_by(doesThis, intoThis) do
      type_call_does_match?(doesRest, intoRest)
    else
      false
    end
  end

  defp type_call(%{type_funs: funs}=env, name, args) do
    case Map.get(funs, name) do
      nil -> raise %UnknownCall{env: env, name: name, args: args}
      heads when is_list(heads) ->
        args_type = Enum.map(args, &type_of_expr(env, &1))
        IO.inspect {:FUNCY, env}
        case Enum.find(heads, &type_call_does_match?(args_type, &1)) do
          nil -> raise %UnknownCall{env: env, name: name, args: args}
          {_argTypes, matchFn} -> matchFn.(args)
        end
    end
  end

  defp type_var(%{type_bindings: bindings}=env, name, scope) do
    case Map.get(bindings, name) do
      nil -> raise %UnknownVar{env: env, name: name, scope: scope}
      type -> type
    end
  end



  defp unify_types!(env, t0, t1) do
    case unify_types(env, t0, t1) do
      nil -> raise %UnificationError{type0: t0, type1: t1}
      type -> type
    end
  end

  defp unify_types(env, t0, t1)
  defp unify_types(env, t, t), do: {env, t}
  defp unify_types(env, {@type_ptr, ptr, _ptrMeta}=t0, t1) do
    case env.type_vars[ptr] do
      nil -> nil
      t -> unify_types(env, t, t1)
    end
  end
  defp unify_types(env, t0, {@type_ptr, ptr, _ptrMeta}=t1) do
    case env.type_vars[ptr] do
      nil -> nil
      t -> unify_types(env, t0, t)
    end
  end
  defp unify_types(env, {@type_generic, nil, _genMeta0}, t1), do: t1
  defp unify_types(env, t0, {@type_generic, nil, _genMeta1}), do: t0
  defp unify_types(env, {tt, v, _m0}=t0, {tt, v, _m1}=_t1) do
    # TODO:  Add in meta parsing
    t0
  end
  defp unify_types(env, t0, t1) do
    nil
  end


  # Reification

  # Literal value
  defp reify_ml_expr(env, {@tag_lit, _lit_meta, lit_value}), do: lit_value

  # Function call
  defp reify_ml_expr(env, {@tag_call, call_meta, [call_name | call_args]}) do
    args = Enum.map(call_args, &reify_ml_expr(env, &1))
    {call_name, call_meta, args}
  end

  # Let binding
  defp reify_ml_expr(env, {@tag_let, let_meta, [binding, exprInner, exprAfter]}) do
    {:__block__, let_meta, [
      {:=, [], [reify_ml_expr(env, binding), reify_ml_expr(env, exprInner)]},
      reify_ml_expr(env, exprAfter)
      ]}
  end

  # Variable binding
  defp reify_ml_expr(env, {@tag_var, bind_meta, [name, scope]}) do
    {name, bind_meta, scope}
  end

end
