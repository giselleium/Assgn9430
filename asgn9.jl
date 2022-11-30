# ExprC structs 

struct NumC
    n
end

struct StrC
    s::String
end

struct IdC
    i::Symbol
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
    args::Array{Symbol}
    body
end

struct PrimC
    o::Symbol
    l
    r
end

struct ErrC
    v
end

ExprC = Union{NumC,StrC,IdC,IfC,AppC,LamC,PrimC,ErrC}

# Value structs 
struct NumV
    num::Real
end

struct StrV
    str::String
end

struct BoolV
    bool::Bool
end

struct ClosV
    args::Array{Symbol}
    body::ExprC
    env
end

struct PrimV
    args::Array{Symbol}
    body::ExprC
end

Value = Union{NumV,StrV,BoolV,ClosV,PrimV}

# Bind Type
struct Bind
    name
    value
end

struct Enviroment
    Bind[]
end
# takes a binding and an environment and returns the given environment
# with the binding added

extendEnv(b::Bind, env::Enviroment) = push!(env, b)

# takes a list of closure arguments, a list of application arguments,
# a closure environment, and an application environment and adds the bindings
# to the closure environment

# Use Strings for Symbols? Have not finished this method

extendEnvMult(cArgs::Array{String}, fArgs::Array{ExprC}, cEnv::Enviroment, fEnv::Enviroment) =
    if isempty(cArgs)
        "extend-env-mult too many arguments provided (JYSS)"
    elseif isempty(fArgs)
        "extend-env-mult too many arguments provided (JYSS)"
    else
        extendEnvMult() # haven't implemented yet
    end

#takes a symbol and an environment and returns the value
#of the binding that has that symbol, if there is one

lookup(target::String, env::Array{Bind}) =
    if isempty(env)
        println("name not found")
    else
        if target == env[1].name
            env[1].value
        else
            popfirst!(env)
            lookup(target, env)
        end
    end

using Test

@test lookup("a", [Bind("b", "first value"), Bind("b", "second value"), Bind("a", "third value")]) == "third value"
@test lookup("b", [Bind("a", "first value"), Bind("b", "second value"), Bind("a", "third value")]) == "second value"
@test lookup("c", [Bind("c", "first value"), Bind("a", "second value"), Bind("b", "third value")]) == "third value"

# (define (lookup [for : Symbol] [env : Environment]) : Value
#   (cond
#     [(empty? env) (error 'lookup "name not found (JYSS)")]
#     [else (cond
#             [(equal? for (bind-name (first env)))
#              (bind-val (first env))]
#             [else (lookup for (rest env))])]))
# Tests

x = NumC(10)
y = IfC(10, 11, 12)
println(x)
println(y)

b = Bind("Test Bind", "Value of 3")
println(b)
