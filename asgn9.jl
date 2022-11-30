

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

struct PrimC
    o
    l
    r
end

struct ErrC
    v
end

ExprC = Union{NumC,StrC,IdC,IfC,LamC,PrimC,ErrC}

# Value structs 

struct NumV
    num
end

struct StrV
    str
end

struct BoolV
    bool
end

struct ClosV
    args
    body
    env
end

struct PrimV
    args
    body
end

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


# takes a value and returns the serialization of the value
function serialize(v)
    if (typeof(v) == NumV)
        string(v.num)
    elseif (typeof(v) == StrV)
        string(v.str)
    elseif (typeof(v) == BoolV)
        if (v.bool)
            "true"
        else
            "false"
        end
    elseif (typeof(v) == ClosV)
        "#<procedure>"
    else
        "#<primop>"
    end
end

println(serialize(NumV(10)))
println(serialize(NumV(10)) == "10")



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
# Tests

x = NumC(10)
y = IfC(10, 11, 12)
println(x)
println(y)

b = Bind("Test Bind", "Value of 3")
println(b)
