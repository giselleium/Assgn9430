
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

# testing making struct instances 
x = NumC(10)
y = IfC(10, 11, 12)
println(x)
println(y)
