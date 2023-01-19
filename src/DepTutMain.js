// A Málà interpreter

use strict

// Common

function Just(x)
{
    return {
        tag: "Just"
        value: x
    }
}

function Nothing()
{
    return {
        tag: "Nothing"
    }
}

// * Abstract syntax:


// TermInferable:

function TermAnnotated(t1, t2) {
    return {
        tag: "TermAnnotated",
        t1: t1,
        t2: t2
    }
}

function TermStar() {
    return {
        tag: "TermStar"
    }
}

function TermPi(t1, t2) {
    return {
        tag: "TermPi",
        t1: t1,
        t2: t2
    }
}

function TermBound(i) {
    return {
        tag: "TermBound",
        i: i
    }
}

function TermFree(name) {
    return {
        tag: "TermFree",
        name: name
    }
}

function TermApplication(t1, t2) {
    return {
        tag: "TermApplication",
        t1: t1,
        t2: t2
    }
}

function eqTermInferable(ti1, ti2)
{
    if (ti1.tag !== ti2.tag)
        return false

    switch (ti1.tag)
    {
        case "TermAnnotated":
        return eqTermInferable(tc1.ti, tc2.ti)

        case "TermStar":
        return true

        case "TermPi":
        return eqTermCheckable(ti1.t1, ti2.t1) && eqTermCheckable(ti1.t2, ti2.t2)

        case "TermBound":
        return ti1.i === ti2.i

        case "TermFree":
        return eqName(ti1.name, ti2.name)

        case "TermApplication":
        return eqTermInferable(ti1.t1, ti2.t1) && eqTermCheckable(ti1.t2, ti2.t2)
    }
}

// TermCheckable:

function TermInferred(ti) {
    return {
        tag: "TermInferred",
        ti: ti
    }
}

function TermLambda(t) {
    return {
        tag: "TermLambda",
        t: t
    }
}

function eqTermCheckable(tc1, tc2)
{
    if (tc1.tag !== tc2.tag)
        return false

    switch (tc1.tag)
    {
        case "TermInferred":
        return eqTermInferable(tc1.ti, tc2.ti)

        case "TermLambda":
        return eqTermCheckable(tc1.t, tc2.t)
    }
}

// Name:

function NameGlobal(name) {
    return {
        tag: "NameGlobal",
        name: name
    }
}

function NameLocal(i) {
    return {
        tag: "NameLocal",
        i: i
    }
}

function NameQuote(i) {
    return {
        tag: "NameQuote",
        i: i
    }
}

function eqName(n1, n2)
{
    if (n1.tag !== n2.tag)
        return false

    switch(n1.tag)
    {
        case "NameGlobal":
        return n1.name == n2.name

        case "NameLocal":
        return n1.i == n2.i

        case "NameQuote":
        return n1.i == n2.i
    }
}

// Value:

function ValueLambda(fn) {
    return {
        tag: "ValueLambda",
        fn: fn
    }
}

function ValueStar() {
    return {
        tag: "ValueStar"
    }
}

function ValuePi(ix, fn) {
    return {
        tag: "ValuePi",
        ix: ix,
        fn: fn
    }
}

function ValueNeutral(neutral) {
    return {
        tag: "ValueNeutral",
        neutral: neutral
    }
}

// Neutral:

function NeutralFree(name) {
    return {
        tag: "NeutralFree",
        name: name
    }
}

function NeutralApplication(neutral, value) {
    return {
        tag: "NeutralApplication",
        neutral: neutral,
        value: value,
    }
}

// Evaluation

function vfree(name) {
    return ValueNeutral(NeutralFree(name))
}

function evalInferable(env, ti) {
    switch (ti.tag)
    {
        case "TermAnnotated":
        return evalCheckable(env, ti.t1)
        break

        case "TermStar":
        return ValueStar()
        break

        case "TermPi":
        return ValuePi(evalCheckable(env, ti.t1) ((x) => evalCheckable ([x].concat(env), ti.t2)))
        break

        case "TermBound":
        return env[ti.i]
        break

        case "TermFree":
        return vfree(ti.name)
        break

        case "TermApplication":
        fnValue = evalInferable(env, ti.t1)
        switch (fnValue.tag) {
            case "ValueLambda":
            return fn.Value.fn(evalCheckable(env, ti.t2))
            break

            case "ValueNeutral":
            return ValueNeutral(NeutralApplication(fnValue.neutral, evalCheckable(env, ti.t2)))
            break

            case "ValuePi":
            throw new Error("impossible: evalInferable ValuePi in Application case")
            break

            case "ValueStar":
            throw new Error("impossible: evalInferable ValueStar in Application case")
            break
        }
        break
    }
}

function evalCheckable(env, tc){
    switch (tc.tag)
    {
        case "TermInferred":
        return evalInferable(env, tc.ti)
        break
        case "TermLambda":
        return ValueLambda( (v) => evalCheckable([v].concat(env), tc.fn))
        break
    }
}


// Type Checking:


function lookupContext(name, context)
{
    var result = context.find(entry => eqName(entry[0], name))
    if result === undefined
    {
        return Nothing()
    } else
    {
        return Just(result[1])
    }
}

function typeInfer(i, context, ti)
{
    switch (ti.tag)
    {
        case "TermAnnotated":
        typeCheck(i, context, ti.t2, ValueStar())
        var t = evalCheckable([], ti.t2)
        typeCheck(i, context, ti.t1, t)
        return t
        break

        case "TermStar":
        return TermStar()
        break

        case "TermPi":
        typeCheck(i, context, ti.t1, ValueStar())
        var t = evalCheckable([], ti.t1)
        typeCheck(
            i+1,
            [[NameLocal(i), t]].concat(context),
            substituteCheckable(0, TermFree(NameLocal(i), ti.t2)),
            ValueStar()
        )
        return ValueStar()
        break

        case "TermFree":
        var t = lookupContext(ti.name, context)
        switch (t.tag)
        {
            case "Just":
            return t.value

            case "Nothing":
            throw new Error("unknown identifier")
        }
        break

        case "TermApplication":
        var s = typeInfer(i, context, ti.t1)
        switch (s.tag)
        {
            case "ValuePi":
            typeCheck(i, context, ti.t2, s.t1)
            return s.t2(evalCheckable([], ti.t2))

            default:
            throw new Error("illegal application")
        }
        break

        case "TermBound":
        throw new Error("typeInfer: impossible? (TermBound)")
    }
}

function typeCheck(i, context, tc, t)
{
    switch (tc.tag)
    {
        case "TermInferred":
        var t_ = typeInfer(i, context, tc.ti)
        if (eqTermCheckable(quote(0, t), quote(0, t_)))
            return
        else
            throw new Error("type mismatch")

        case "TermLambda":
        switch(t.tag)
        {
            case "ValuePi":
            typeCheck(
                i+1,
                [[NameLocal(i), t.ix]].concat(context),
                substituteCheckable(0, TermFree(NameLocal(i)), tc.t),
                t.fn(vfree(NameLocal(i)))
            )

            default:
            throw new Error("type mismatch")
        }

        default:
        throw new Error("type mismatch")
    }
}

function substituteCheckable(i, r, tc)
{
    switch (tc.tag)
    {
        case "TermInferred":
        return TermInferred(substituteInferable(i, r, tc.ti))

        case "TermLambda":
        return TermLambda(substituteCheckable(i+1, r, tc.t))
    }
}

function substituteInferable(i, r, ti)
{
    switch (ti.tag)
    {
        case "TermAnnotated":
        return TermAnnotated(substituteCheckable(i,r, ti.t1), ti.t2)

        case "TermStar":
        return TermStar()

        case "TermPi":
        return TermPi(substituteCheckable(i,r,ti.t1), substituteCheckable(i+1, r ti.t2))

        case "TermBound":
        if (i == ti.i)
            return r
        else
            return TermBound(ti.i)

        case "TermFree":
        return TermFree(ti.name)

        case "TermApplication":
        return TermApplication(
            substituteInferable(i, r, ti.t1),
            substituteInferable(i, r, ti.t2)
        )
    }
}

// Quoting

function boundfree(i, name)
{
    switch (name.tag)
    {
        case "NameQuote":
        return TermBound(i - name.i - 1)

        default:
        return TermFree(name)
    }
}

function quote(i, value)
{
    switch(value.tag)
    {
        case "ValueLambda":
        return TermLambda(quote(i+1), value.fn(vfree(NameQuote(i))))

        case "ValueStar":
        return TermInferred(TermStar())

        case "ValuePi":
        return TermInferred(TermPi(quote(i, value.ix), quote(i+1, value.fn(vfree(NameQuote(i))))))

        case "ValueNeutral":
        return TermInferred(neutralQuote(i, value.neutral))
    }
}

function neutralQuote(i, neutral)
{
    switch (neutral.tag)
    {
        case "NeutralFree":
        return boundfree(i, neutral.name)

        case "NeutralApplication":
        return TermApplication(neutralQuote(i, neutral.neutral), quote(i, neutral.value))
    }
}

// A top-level entry point that can evaluate values of type: ∀Unit:*. (printHelloWord : Unit) -> Unit
function entryPoint(term)
{
    // todo
}
