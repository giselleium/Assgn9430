
# ExprC structs 
struct NumC
    n
end
struct StrC
    s
end
struct IdC
    i
end
struct IfC
    test
    then
    otherwise
end
struct AppC
    app
    args
end
struct LamC
    args
    body
end

# ; takes a JYSS5 expression and an environment
# ; and returns the value the expression evaluates to
# (define (interp [e : ExprC] [env : Environment]) : Value
#   (match e
#     [(numC n) (numV n)]
#     [(strC s) (strV s)]
#     [(idC s) (lookup s env)]
#     [(appC f a) (match (interp f env)
#                   [(? closV? c)(local ([define f-value c])
#                   (interp (closV-body f-value)
#                           (extend-env-mult (closV-args f-value) a (closV-env f-value) env)))]
#                   [(? primV? p)(local ([define f-value (closV (primV-args p) (primV-body p) env)])
#                   (interp (closV-body f-value)
#                           (extend-env-mult (closV-args f-value) a (closV-env f-value) env)))]
#                   [other (error 'interp "application of a non-closure (JYSS)")])]
#     [(primC o l r) (interp-prim o (interp l env) (interp r env))]
#     [(errC v) (error 'user-error (serialize (interp v env)))]
#     [(ifC a b c) (match (interp a env)
#                    [(boolV ?) (cond
#                                 [? (interp b env)]
#                                 [else (interp c env)])]
#                    [other (error 'interp "non-boolean provided as conditional (JYSS)")])]
#     [(lamC a b) (closV a b env)]))

# testing making struct instances 
x = NumC(10)
y = IfC(10, 11, 12)
println(x)
println(y)
