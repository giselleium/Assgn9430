using Test

#primodial block
begin
    # ExprC structs 
    struct NumC
        n
    end

    struct StrC
        s::String
    end

    struct IdC
        i::String
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
        o::String
        l
        r
    end

    struct ErrC
        v
    end

    ExprC = Union{NumC,StrC,IdC,IfC,LamC,PrimC,ErrC}

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
        args
        body
        env
    end

    struct PrimV
        args::Array{String}
        body::ExprC
    end

    Value = Union{NumV,StrV,BoolV,ClosV,PrimV}

    # Bind Type
    struct Bind
        name
        value::Value
    end

    struct Environment
        b::Vector{Bind}
    end

    topEnv = Environment([
        Bind("true", BoolV(true)),
        Bind("false", BoolV(false)),
        Bind("+", PrimV(["a", "b"], PrimC("+", IdC("a"), IdC("b")))),
        Bind("-", PrimV(["a", "b"], PrimC("-", IdC("a"), IdC("b")))),
        Bind("*", PrimV(["a", "b"], PrimC("*", IdC("a"), IdC("b")))),
        Bind("/", PrimV(["a", "b"], PrimC("/", IdC("a"), IdC("b")))),
        Bind("<=", PrimV(["a", "b"], PrimC("<=", IdC("a"), IdC("b")))),
        Bind("equal?", PrimV(["a", "b"], PrimC("equal?", IdC("a"), IdC("b")))),
        Bind("error", PrimV(["v"], ErrC(IdC("v"))))
    ])



    # takes a binding and an environment and returns the given environment
    # with the binding added

    extendEnv(b::Bind, env::Environment) = push!(env, b)


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
    @test serialize(NumV(10)) == "10"
    @test serialize(BoolV(false)) == "false"
    @test serialize(ClosV(1, 2, 3)) == "#<procedure>"

    # takes a symbol and a left and right value and returns
    # the result of the operation the symbol represents on the two given values
    function interpPrim(o::String, l::Value, r::Value)
        if (typeof(l) == NumV && typeof(r) == NumV)
            if (o == "+")
                NumV(l.num + r.num)
            elseif (o == "-")
                NumV(l.num - r.num)
            elseif (o == "*")
                NumV(l.num * r.num)
            elseif (o == "/")
                if (r.num == 0)
                    throw(DomainError(o, "interpPrim division by zero (JYSS5)"))
                else
                    NumV(l.num / r.num)
                end
            elseif (o == "<=")
                BoolV(l.num <= r.num)
            elseif (o == "equal?")
                if (typeof(l) == ClosV ||
                    typeof(l) == PrimV ||
                    typeof(r) == ClosV ||
                    typeof(r) == PrimV)
                    BoolV(false)
                else
                    BoolV(l.num == r.num)
                end
            end
        else
            throw(DomainError(o, "interpPrim invalid input (JYSS5)"))
        end
    end
    @test interpPrim("+", NumV(19), NumV(3)) == NumV(22)
    @test interpPrim("/", NumV(20), NumV(4)) == NumV(5.0)
    @test interpPrim("<=", NumV(20), NumV(4)) == BoolV(false)
    @test interpPrim("<=", NumV(4), NumV(10)) == BoolV(true)
    @test interpPrim("equal?", NumV(4), NumV(10)) == BoolV(false)

    # takes a JYSS5 expression and an environment
    # and returns the value the expression evaluates to
    function interp(e::ExprC, env::Environment)
        if (typeof(e) == NumC)
            NumV(e.n)
        elseif (typeof(e) == StrC)
            StrV(e.s)
        elseif (typeof(e) == IdC)
            lookup(e.i, env)
        elseif (typeof(e) == AppC)


            # elseif (typeof(e) == PrimC)
            # elseif (typeof(e) == ErrC)
            # elseif (typeof(e) == IfC)
            # elseif (typeof(e) == LamC)
            # else
        end
    end
    @test interp(NumC(10), topEnv) == NumV(10)
    @test interp(StrC("10"), topEnv) == StrV("10")


    # takes a list of closure arguments, a list of application arguments,
    # a closure environment, and an application environment and adds the bindings
    # to the closure environment

    # Use Strings for Symbols? Have not finished this method

    extendEnvMult(cArgs::Array{String}, fArgs::Array{ExprC}, cEnv::Environment, fEnv::Environment) =
        if isempty(cArgs)
            "extend-env-mult too many arguments provided (JYSS)"
        elseif isempty(fArgs)
            "extend-env-mult too many arguments provided (JYSS)"
        else
            extendEnvMult() # haven't implemented yet
        end

    #takes a symbol and an environment and returns the value
    #of the binding that has that symbol, if there is one

    lookup(target::String, env::Vector{Bind}) =
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

    @test lookup("a", [Bind("b", StrV("first value")), Bind("b", StrV("second value")), Bind("a", StrV("third value"))]) == StrV("third value")
    @test lookup("b", [Bind("a", StrV("first value")), Bind("b", StrV("second value")), Bind("a", StrV("third value"))]) == StrV("second value")
    @test lookup("c", [Bind("b", StrV("first value")), Bind("a", StrV("second value")), Bind("c", StrV("third value"))]) == StrV("third value")
end