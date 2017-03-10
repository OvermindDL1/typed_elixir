defmodule MLElixirOld do
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
    defstruct counter: -1, funs: %{}, type_funs: %{}, type_bindings: %{}, type_vars: %{}
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
    defexception message: "Unknown variable accessed", env: %MLEnv{}, name: "<unknown>", scope: [], meta: nil
    def message(e) do
      line = if e.meta === nil, do: -1, else: e.meta[:line] || -1
      "#{line}:Access of unknown var `#{e.name}`"
    end
  end

  defmodule InvalidCall do
    defexception message: "Unknown reason", name: "<unknown>", meta: nil
    def message(e) do
      line = if e.meta === nil, do: -1, else: e.meta[:line] || -1
      "#{line}:Invalid call of `#{e.name}` because of:  #{e.message}"
    end
  end

  # TODO:  Use the message arg around
  defmodule UnificationError do
    defexception message: "<unknown>", env: %MLEnv{}, type0: nil, type1: nil
    def message(e) do
      "Unification error between `#{inspect e.type0}` and `#{inspect e.type1}` with message:  #{e.message}"
    end
  end




  # Types
  # @type_const_ :"$$TCONST$$" # {@type_const, atom,                  meta}
  # @type_app   :"$$TAPP$$"   # {@type_app,   type,          [type], meta}
  # @type_func  :"$$TFUNC$$"  # {@type_func,  [type],         type,  meta}
  # @type_var   :"$$TVAR$$"   # {@type_var,   env.type_vars,         meta}
  #
  # # env.type_vars
  # @type_var_unbound :"$$TVARUNBOUND$$" # {@type_var_unbound, string, int, meta}
  # @type_var_link    :"$$TVARLINK$$"    # {@type_var_link,    type,        meta}
  # @type_var_generic :"$$TVARGENERIC$$" # {@type_var_generic, string,      meta}

  @tag_lit       :"$$LIT$$"
  @tag_var       :"$$VAR$$"
  @tag_let       :"$$LET$$"
  @tag_mbind     :"$$MULTIBIND$$"
  @tag_call      :"$$CALL$$"
  @tag_func      :"$$FUNC$$"
  @tag_func_head :"$$FUNCHEAD$$"

  @type_const     :"$$TCONST$$"    # {@type_const, type, meta}
  @type_ptr       :"$$TPTR$$"      # {@type_ptr, ptr, meta}
  # @type_or        :"$$TPTR$$"      # {@type_or, [types], meta}
  @type_func      :"$$TFUNC$$"     # {@type_func, [heads], meta}
  @type_func_head :"$$TFUNCHEAD$$" # {@type_func_head, {args, return}, meta}

  # @type_ptr_generic :"$$TGENERIC$$" # {@type_ptr_generic, nil, meta}
  @type_ptr_unbound :"$$TUNBOUND$$" # {@type_ptr_unbound, nil, meta}



  @module_open_func :__ml_open__


  # TODO: Add the disc_union library or something to replace the above constants so they are actually useful elsewhere...


  defmodule Core do

    def __ml_open__ do
      %{
        funs: %{
          +: fn # Passes in the environment, meta of the call, and the calls arguments AST's
            (env, meta, [{_,leftMeta,_}, {_,rightMeta,_}]=args) ->
              left = leftMeta[:type]
              right = rightMeta[:type]
              {typeTag, type, _typeMeta} = MLElixir.unify_types!(env, left, right)
              # TODO:  Make addition refine the values properly on the type
              if typeTag === :"$$TCONST$$" and type in [:int, :float] do
                ast = {:"$$CALL$$", [type: {typeTag, type, []}] ++ meta, [{Kernel, :+} | args]}
                {env, ast}
              else
                raise %InvalidCall{message: "Invalid arguments types, only `int` or `float` is allowed", name: :+, meta: meta}
              end
            end,
          },
        types: %{
        },
      }
    end

  end




  defmacro defml(opts) when is_list(opts) do
    case opts[:do] do
      nil ->
        [ast] = opts
        defml_impl(ast, [])
      ast when is_tuple(ast) -> defml_impl(ast, opts)
    end
  end
  defmacro defml(expr) do
    defml_impl(expr, [])
  end

  defp defml_impl(expr, opts) do
    # IO.inspect {:ML, expr}
    no_default_opens = opts[:no_default_opens] || false
    env = if no_default_opens, do: %MLEnv{}, else: open_module(%MLEnv{}, MLElixir.Core)
    env = case opts[:open] do
      nil -> env
      module when is_atom(module) ->
        open_module(env, module)
      modules when is_list(modules) ->
        Enum.reduce(modules, env, fn(module, env) -> open_module(env, module) end)
    end
    {ml_env, ml_ast} = parse_ml_expr(env, expr)
    # IO.inspect {:MLAST, ml_ast}
    # IO.inspect {:MLENV, ml_env}
    ast = reify_ml_expr(ml_env, ml_ast)
    # IO.inspect {:MLDONE, ast}
    ast
  end



  defp parse_ml_expr(env, expr)

  # Literals... why does Elixir not give line information for these?!?
  defp parse_ml_expr(env, int)   when is_integer(int), do: {env, {@tag_lit, [type: {@type_const, :int,   [values: [int]]}],   int}}
  defp parse_ml_expr(env, float) when is_float(float), do: {env, {@tag_lit, [type: {@type_const, :float, [values: [float]]}], float}}
  defp parse_ml_expr(env, atom)  when is_atom(atom),   do: {env, {@tag_lit, [type: {@type_const, :atom,  [values: [atom]]}],   atom}}

  # defp parse_ml_expr(env, {:module, module_meta, [[{module_name, module_defs}]]}) when is_atom(module_name) do
  #   defs = Enum.map(module_defs, &parse_module_def(&1, env))
  #   IO.inspect {:MODULE, module_name, defs}
  #   :todo
  # end

  # TODO:  Add `rec`/`and` handling here

  # Let variable binding to expression with following
  defp parse_ml_expr(env, {:let, let_meta, let_ast}) do
    parse_let(env, let_meta, let_ast)
  end
  # defp parse_ml_expr(env, {:let, let_meta, [{:=, equal_meta, [binding_ast, {:in, in_meta, [inner_expr_ast, after_expr_ast]}]}]}) do
  #   {envBinding, binding} = parse_binding(env, binding_ast)
  #   {envExprInner, exprInner} = parse_ml_expr(envBinding, inner_expr_ast)
  #   {envInnerResolved, bindingResolved} = resolve_binding(envExprInner, binding, exprInner)
  #   {envExprAfter, exprAfter} = parse_ml_expr(envInnerResolved, after_expr_ast)
  #   ast = {@tag_let, [type: type_of_expr(envExprAfter, exprAfter)] ++ let_meta, [bindingResolved, exprInner, exprAfter]}
  #   {envExprAfter, ast}
  # end

  # # Let variable binding to expression without following
  # defp parse_ml_expr(env, {:let, let_meta, [{:=, equal_meta, [binding_ast, expr_ast]}]}) do
  #   {envInner, binding} = parse_binding(binding_ast)
  # end

  # A variable lookup
  defp parse_ml_expr(env, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
    var_type = type_binding(env, name, scope)
    ast = {@tag_var, [type: var_type] ++ meta, [name, scope]}
    {env, ast}
  end

  # A function definition
  defp parse_ml_expr(env, {:->, body_meta, [[{:fun, fun_meta, [head_ast]}], body_ast]}) do
    {env, {_, headMeta, _} = head} = parse_fn_head(env, head_ast)
    type = {@type_func, headMeta[:type], []}
    ast = {@tag_func, [type: type] ++ fun_meta, [head]}
    {env, ast}
  end

  # A function call
  defp parse_ml_expr(env, {name, call_meta, call_args}) when is_atom(name) and is_list(call_args) do
    case env.funs[name] do
      nil ->
        case env.type_bindings[name] do # TODO:  Combine the func and bindings maps...
          nil -> raise %InvalidCall{message: "No such function found", name: name, meta: call_meta}
          {@type_ptr, ptr, _meta} ->
            case env.type_vars[ptr] do
              {@type_func, heads, _funcMeta} ->
                # TODO:  Find matching head?  Or change @type_func to give an anonymous function like env.funs does?
                :TODO
              _ -> raise %InvalidCall{message: "Trying to call a non-function", name: name, meta: call_meta}
            end
          _var -> raise %InvalidCall{message: "Found var of unknown type", name: name, meta: call_meta}
        end
      fun ->
        args = Enum.map(call_args, &elem(parse_ml_expr(env, &1), 1))
        {env, ast} = fun.(env, call_meta, args)
        {env, ast}
    end
  end



  defp parse_fn_head(env, {:->, meta, [args, expr]}) do
    {envBinding, bindings} = Enum.reduce(args, {env, []}, fn(binding_ast, {e, a}) ->
      {en, binding} = parse_binding(e, binding_ast)
      {en, a ++ [binding]}
    end)
    {_envExprInner, {_,bodyMeta,_}=exprInner} = parse_ml_expr(envBinding, expr)
    # TODO:  Parse bindings and expression based on the bindings
    args_types = Enum.map(bindings, &(elem(&1,1)[:type]))
    return_type = bodyMeta[:type]
    type = {@type_func_head, {args_types, return_type}, []}
    {@tag_func_head, [type: type] ++ meta, [bindings, exprInner]}
  end



  # defp parse_module_def({:=, equal_meta, [name_ast, expr]}, env) do
  #   :blah
  # end



  defp parse_let(env, let_meta, ast)

  # binding no name
  defp parse_let(env, let_meta, [{:=, _equal_meta, [binding_ast, {:in, _in_meta, [inner_expr_ast, after_expr_ast]}]}]) do
    {envBinding, binding} = parse_binding(env, binding_ast)
    {envExprInner, exprInner} = parse_ml_expr(envBinding, inner_expr_ast)
    # IO.inspect {:BLORP, envExprInner.type_vars, binding, exprInner}
    {envInnerResolved, bindingResolved} = resolve_binding(envExprInner, binding, exprInner)
    # IO.inspect {:BLEEP, envInnerResolved.type_vars, bindingResolved}
    {envExprAfter, exprAfter} = parse_ml_expr(envInnerResolved, after_expr_ast)
    ast = {@tag_let, [type: type_of_expr(envExprAfter, exprAfter)] ++ let_meta, [bindingResolved, exprInner, exprAfter]}
    {envExprAfter, ast}
  end

  # binding with name
  defp parse_let(env, let_meta, [[{name_ast, {:=, equal_meta, [binding_ast, {:in, _in_meta, [inner_expr_ast, after_expr_ast]}]}}]]) when is_atom(name_ast) do
    {envBinding, {_, bindMeta, _}=binding} = parse_binding(env, binding_ast)
    {envBinding, nameType} = env_storeBinding(envBinding, name_ast, bindMeta[:type])
    name = {@tag_var, [type: nameType] ++ let_meta, [name_ast, nil]}
    args = {@tag_mbind, [type: nameType] ++ equal_meta, [name, binding]}
    {envExprInner, exprInner} = parse_ml_expr(envBinding, inner_expr_ast)
    {envInnerResolved, bindingResolved} = resolve_binding(envExprInner, args, exprInner)
    {envExprAfter, exprAfter} = parse_ml_expr(envInnerResolved, after_expr_ast)
    ast = {@tag_let, [type: type_of_expr(envExprAfter, exprAfter)] ++ let_meta, [bindingResolved, exprInner, exprAfter]}
    {envExprAfter, ast}
  end

  # open a module into here
  defp parse_let(env, _let_meta, [{:open, _open_meta, [{:in, _in_meta, [module_ast, expr_ast]}]}]) do
    {env, module} = parse_modulename(env, module_ast)
    env = open_module(env, module)
    parse_ml_expr(env, expr_ast)
  end


  defp parse_modulename(env, module_ast)
  defp parse_modulename(env, atom) when is_atom(atom), do: {env, atom}
  defp parse_modulename(env, {:__aliases__, _meta, atomList}) when is_list(atomList), do: {env, Module.concat(atomList)}



  defp open_module(env, module, opts \\ [])
  defp open_module(env, module, opts) when is_atom(module) do
    case Code.ensure_compiled(module) do
      {:error, reason} -> raise %InvalidCall{message: "Module is not loadable", name: module, meta: reason}
      {:module, _} ->
        case function_exported?(module, @module_open_func, 0) do
          false -> raise %InvalidCall{message: "Module does not exist or is not an ML module", name: module}
          true ->
            moduleDef = apply(module, @module_open_func, [])
            moduledef_into_env(env, moduleDef, opts)
        end
    end
  end

  defp moduledef_into_env(env, moduleDef, _opts) do
    funs = Map.merge(env.funs, moduleDef.funs)
    %{env |
      funs: funs
    }
  end



  defp env_storeBinding(env, name, ptr_type \\ {@type_ptr_unbound, nil, []}) do
    env = increment_counter(env)
    type = {@type_ptr, env.counter, []}
    # Hmm, cancel this, still bind them even if unused, good for lookups later to ensure typing
    # if String.starts_with?(to_string(name), "_") do
    #   {env, type}
    # else
      {
        %{env |
          type_bindings: Map.put(env.type_bindings, name, type),
          type_vars: Map.put(env.type_vars, env.counter, ptr_type)
          },
        type
      }
    # end
  end


  # STATIC BINDING: Typeless variable binding
  # TODO:  Make version of this where scope defines the function
  defp parse_binding(env, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
    {newEnv, type} = env_storeBinding(env, name)
    ast = {@tag_var, [type: type] ++ meta, [name, scope]}
    {newEnv, ast}
  end
  # MATCHING BINDING:  Constants - Copied from parse_ml_expr...
  defp parse_binding(env, int)   when is_integer(int), do: {env, {@tag_lit, [type: {@type_const, :int,   [values: [int]]}],   int}}
  defp parse_binding(env, float) when is_float(float), do: {env, {@tag_lit, [type: {@type_const, :float, [values: [float]]}], float}}
  defp parse_binding(env, atom)  when is_atom(atom),   do: {env, {@tag_lit, [type: {@type_const, :atom,  [values: [atom]]}],   atom}}
  # STATIC TYPED BINDING:  Typed variable binding
  defp parse_binding(env, {:!, meta, [[{name, type_expr}]]}) when is_atom(name) do
    {env, varType} = parse_type(env, name, type_expr)
    {newEnv, type} = env_storeBinding(env, name, varType)
    ast = {@tag_var, [type: type] ++ meta, [name, nil]} # TODO:  Really need to constrain the scopes well...  `nil` is wide open
    {newEnv, ast}
  end


  defp parse_type(env, varname, type_ast)
  # Parse a constant int/float/atom type
  defp parse_type(env, varname, {name, _meta, constraints}) when name in [:int, :float, :atom] do
    {env, values} = parse_type_const_constraints(env, varname, name, constraints)
    {env, {@type_const, name, values}}
  end

  defp parse_type_const_constraints(env, varname, ptype, constraint)
  defp parse_type_const_constraints(env, _varname, _ptype, scope) when is_atom(scope), do: {env, []}
  defp parse_type_const_constraints(env, varname, :int, [{:=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_integer(val), do: {env, [values: [val]]}
  defp parse_type_const_constraints(env, varname, :int, [{:>=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_integer(val), do: {env, [values: [{val, :infinite}]]}
  defp parse_type_const_constraints(env, varname, :int, [{:<=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_integer(val), do: {env, [values: [{:infinite, val}]]}
  defp parse_type_const_constraints(env, varname, :float, [{:=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_float(val), do: {env, [values: [val]]}
  defp parse_type_const_constraints(env, varname, :float, [{:>=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_float(val), do: {env, [values: [{val, :infinite}]]}
  defp parse_type_const_constraints(env, varname, :float, [{:<=, _meta, [{varname,_,varnameScope}, val]}]) when is_atom(varnameScope) and is_float(val), do: {env, [values: [{:infinite, val}]]}



  # TODO:  Add support for matching bindings here

  # The binding is but a single variable, resolve it
  defp resolve_binding(env, {@tag_var, bindMeta, [_name, _scope]} = binding, {_, exprMeta, _}) do
    bindType = bindMeta[:type]
    exprType = exprMeta[:type]
    case bindType do
      {@type_ptr, ptr, _typePtrMeta} -> # Bindings should always have a ptr type if not starting with "_"
        case env.type_vars[ptr] do
          nil -> raise %UnificationError{type0: bindType}
          t ->
            tightestType = resolve_types!(env, exprType, t)
            resolvedEnv = %{env |
              type_vars: Map.put(env.type_vars, ptr, tightestType)
            }
            # IO.inspect {:BLERGH, exprType, t, ptr, resolvedEnv, binding}
            {resolvedEnv, binding}
        end
      t -> raise %UnificationError{type0: t, type1: exprType}
    end
  end

  # Literal binding
  defp resolve_binding(env, {@tag_lit, bindMeta, value}, {_, exprMeta, _}) do
    bindType = bindMeta[:type]
    exprType = exprMeta[:type]
    resolvedType = resolve_types!(env, bindType, exprType)
    binding = {@tag_lit, [type: resolvedType] ++ bindMeta, value}
    {env, binding}
  end

  # Multi-bindings
  defp resolve_binding(env, {@tag_mbind, bindMeta, values}, {_, _exprMeta, _}=expr) do
    {env, newValues} = Enum.reduce(values, {env, []}, fn(value, {env, acc}) ->
      {e, v} = resolve_binding(env, value, expr)
      {e, acc ++ [v]}
    end)
    {env, {@tag_mbind, bindMeta, newValues}}
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

  # defp type_is_fullfilled_by({type, _type_meta}, {constraint_type, _constraint_meta}) do
  #   # TODO:  Compare the meta's as necessary too perhaps?
  #   case {type, constraint_type} do
  #     {type, type} -> true
  #     _ -> false
  #   end
  # end

  defp type_of_expr(_env, {_, meta, _}), do: meta[:type]

  # defp type_call_does_match?([], []), do: true
  # defp type_call_does_match?([doesThis | doesRest], [intoThis | intoRest]) do
  #   if type_is_fullfilled_by(doesThis, intoThis) do
  #     type_call_does_match?(doesRest, intoRest)
  #   else
  #     false
  #   end
  # end
  #
  # defp type_call(%{type_funs: funs}=env, name, args) do
  #   case Map.get(funs, name) do
  #     nil -> raise %UnknownCall{env: env, name: name, args: args}
  #     heads when is_list(heads) ->
  #       args_type = Enum.map(args, &type_of_expr(env, &1))
  #       IO.inspect {:FUNCY, env}
  #       case Enum.find(heads, &type_call_does_match?(args_type, &1)) do
  #         nil -> raise %UnknownCall{env: env, name: name, args: args}
  #         {_argTypes, matchFn} -> matchFn.(args)
  #       end
  #   end
  # end

  defp type_binding(%{type_bindings: bindings}=env, name, scope) do
    case Map.get(bindings, name) do
      nil -> raise %UnknownVar{env: env, name: name, scope: scope}
      type -> type
    end
  end



  defp resolve_types_const_meta_values(_env, _type, fromValues, intoValues)
  defp resolve_types_const_meta_values(_env, _type, value, value), do: value
  defp resolve_types_const_meta_values(_env, _type, fromValues, []), do: fromValues # `into` is accepting any value
  defp resolve_types_const_meta_values(_env, _type, fromValues, intoValues) do
    if Enum.all?(fromValues, fn
      {:infinite, :infinite} -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        :infinite -> true
        _ -> false
        end)
      {fromFirst, :infinite} -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        {intoFirst, :infinite} -> fromFirst>=intoFirst
        :infinite -> true
        _ -> false
      end)
      {:infinite, fromLast} -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        {:infinite, intoLast} -> fromLast<=intoLast
        :infinite -> true
        _ -> false
      end)
      {fromFirst, fromLast} -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        {intoFirst, :infinite} -> fromFirst>=intoFirst
        {:infinite, intoLast} -> fromLast<=intoLast
        {intoFirst, intoLast} -> fromFirst>=intoFirst and fromLast<=intoLast
        :infinite -> true
        into -> into === fromFirst and into === fromLast
        end)
      :infinite -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        :infinite -> true
        _ -> false
        end)
      from -> Enum.all?(intoValues, fn
        {:infinite, :infinite} -> true
        {intoFirst, :infinite} -> from>=intoFirst
        {:infinite, intoLast} -> from<=intoLast
        {intoFirst, intoLast} -> from>=intoFirst and from<=intoLast
        :infinite -> true
        into -> from === into
        end)
      end) do
        fromValues
      else
        nil
      end
  end


  def resolve_types!(env, fromType, intoType) do
    # inspect {:RESOLVERINATING, env, fromType, intoType}
    type = resolve_types(env, fromType, intoType)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end
  # `intoType` needs to be able to encompass `fromType`, such as if fromType
  # has a specific value but intoType accepts a range of values that includes
  # that values
  def resolve_types(env, fromType, intoType)
  def resolve_types(_env, type, type), do: type
  def resolve_types(env, {@type_const, type, fromMeta}=fromType, {@type_const, type, intoMeta}=intoType) do
    case resolve_types_const_meta_values(env, type, List.wrap(fromMeta[:values]), List.wrap(intoMeta[:values])) do
      nil ->
        # TODO:  Make a dedicated %ResolutionError{} or something, or something specifically for these values perhaps?
        %UnificationError{message: "Unable to resolve", env: env, type0: fromType, type1: intoType}
      _values -> fromType
    end
  end
  def resolve_types(env, {@type_const, _fromType, _fromMeta}=fromType, {@type_const, _intoType, _intoMeta}=intoType) do
    %UnificationError{message: "Unable to resolve mismatched types", env: env, type0: fromType, type1: intoType}
  end
  def resolve_types(env, fromType, {@type_ptr, ptr, _intoMeta}=intoType) do
    case env.type_vars[ptr] do
      nil -> %UnificationError{message: "Unable to resolve pointed to var due to not being set", env: env, type0: fromType, type1: intoType}
      t -> resolve_types(env, fromType, t)
    end
  end
  def resolve_types(env,  {@type_ptr, ptr, _fromMeta}=fromType, intoType) do
    case env.type_vars[ptr] do
      nil -> %UnificationError{message: "Unable to resolve pointed to var due to not being set", env: env, type0: fromType, type1: intoType}
      t -> resolve_types(env, t, intoType)
    end
  end
  def resolve_types(_env, fromType, {@type_ptr_unbound, nil, _intoMeta}), do: fromType



  defp unify_types_const_meta_values(_env, type, values0, values1)
  defp unify_types_const_meta_values(_env, _type, [], []), do: []
  defp unify_types_const_meta_values(_env, _type, [], values1), do: values1
  defp unify_types_const_meta_values(_env, _type, values0, []), do: values0
  defp unify_types_const_meta_values(_env, _type, values0, values1) do
    values0 ++ values1 # TODO:  Make this smarter, like by combining ranges, right now just combine them all
  end

  defp unify_types_const_meta(env, type, meta0, meta1) do
    unified_values = unify_types_const_meta_values(env, type, List.wrap(meta0[:values]), List.wrap(meta1[:values]))
    if Exception.exception?(unified_values) do
      unified_values
    else
      [values: unified_values] ++ meta0 |> Enum.dedup() # TODO:  Simplify and merge
    end
  end


  def unify_types!(env, t0, t1) do
    type = unify_types(env, t0, t1)
    if Exception.exception?(type) do
      raise type
    else
      type
    end
  end

  def unify_types(env, t0, t1)
  def unify_types(env, t, t), do: {env, t}
  def unify_types(env, {@type_const, type, meta0}, {@type_const, type, meta1}) do
    meta = unify_types_const_meta(env, type, meta0, meta1)
    {@type_const, type, meta}
  end
  def unify_types(env, {@type_ptr, ptr, _ptrMeta}=t0, t1) do
    case env.type_vars[ptr] do
      nil -> %UnificationError{message: "Pointed to type0 is unbound", env: env, type0: t0, type1: t1}
      t -> unify_types(env, t, t1)
    end
  end
  def unify_types(env, t0, {@type_ptr, ptr, _ptrMeta}=t1) do
    case env.type_vars[ptr] do
      nil -> %UnificationError{message: "Pointed to type1 is unbound", env: env, type0: t0, type1: t1}
      t -> unify_types(env, t0, t)
    end
  end
  # def unify_types(_env, {@type_ptr_generic, nil, _genMeta0}, t1), do: t1
  # def unify_types(_env, t0, {@type_ptr_generic, nil, _genMeta1}), do: t0
  def unify_types(_env, {@type_ptr_unbound, nil, _genMeta0}, t1), do: t1
  def unify_types(_env, t0, {@type_ptr_unbound, nil, _genMeta1}), do: t0
  def unify_types(_env, {tt, v, _m0}=t0, {tt, v, _m1}=_t1) do
    # TODO:  Add in meta parsing
    t0
  end
  def unify_types(env, t0, t1) do
    %UnificationError{message: "Unable to unify types", env: env, type0: t0, type1: t1}
  end


  # Reification

  defp reify_ml_expr(env, ast, to \\ :elixir)
  defp reify_ml_expr(env, ast, :elixir), do: reify_ml_expr_elixir(env, ast )

  # Literal value
  defp reify_ml_expr_elixir(_env, {@tag_lit, _lit_meta, lit_value}), do: lit_value

  # Function call
  defp reify_ml_expr_elixir(env, {@tag_call, call_meta, [call_name | call_args]}) do
    args = Enum.map(call_args, &reify_ml_expr_elixir(env, &1))
    case call_name do
      {module, func} ->
        {{:., [], [{:__aliases__, [alias: false], [module]}, func]}, call_meta, args}
    end
  end

  # Let binding
  defp reify_ml_expr_elixir(env, {@tag_let, let_meta, [binding, exprInner, exprAfter]}) do
    {:__block__, let_meta, [
      {:=, [], [reify_ml_expr_elixir(env, binding), reify_ml_expr_elixir(env, exprInner)]},
      reify_ml_expr_elixir(env, exprAfter)
      ]}
  end

  # Variable binding
  defp reify_ml_expr_elixir(_env, {@tag_var, bind_meta, [name, scope]}) do
    {name, bind_meta, scope}
  end

  # Multi-binding
  defp reify_ml_expr_elixir(env, {@tag_mbind, bind_meta, bindings}) do
    List.foldr(bindings, nil, fn
      (binding, nil) -> reify_ml_expr_elixir(env, binding)
      (binding, ast) -> {:=, bind_meta, [reify_ml_expr_elixir(env, binding), ast]}
    end)
  end

  # Function
  defp reify_ml_expr_elixir(env, {@tag_func, meta, heads}) do
    {:fn, meta, Enum.map(heads, &reify_ml_expr_elixir(env, &1))}
  end

  # Function head
  defp reify_ml_expr_elixir(env, {@tag_func_head, meta, [args, body]}) do
    {:->, meta, [Enum.map(args, &reify_ml_expr_elixir(env, &1)), reify_ml_expr_elixir(env, body)]}
  end

end
