namespace Brahma.FSharp.OpenCL.Translator

type State<'state, 'result> = State of ('state -> 'result * 'state)

module State =
    let inline run state (State f) =
        f state

    let exec state (State f) =
        snd (f state)

    let eval state (State f) =
        fst (f state)

    let inline return' x = State <| fun state ->
        (x, state)

    let inline (>>=) x f = State <| fun state ->
        let (y, state') = run state x
        run state' (f y)

    let get = State (fun s -> s, s)

    let put newState = State <| fun _ ->
        (), newState

    // modify state
    let modify f =
        get >>= (f >> put)

    // apply f to state to produce value
    let gets f =
        get >>= (f >> return')

    let map f s = State <| fun state ->
        let (x, state) = run state s
        f x, state

    let using f x = State <| fun state ->
        eval (f state) x, state

    let collect (list: State<'s, 'a> list) =
        list
        |> List.fold
            (fun state elem ->
                state >>= fun state ->
                elem >>= fun elem ->
                return' (elem :: state)
            ) (return' List.empty)
        |> fun args -> map List.rev args

type StateBuilder<'state>() =
    member inline this.Bind(x: State<'state, 'a>, f: 'a -> State<'state, 'b>) = State.(>>=) x f
    member inline this.Return(x: 'a) : State<'state, 'a> = State.return' x
    member inline this.ReturnFrom(x: State<'state, 'a>) = x
    member inline this.Zero() : State<'state, unit> = State.return' ()

    member inline this.Combine(x1: State<'state, _>, x2: State<'state, _>) =
        State <| fun context ->
            let (_, context) = State.run context x1
            State.run context x2

    member inline this.Delay(rest) =
        this.Bind(this.Zero(), (fun () -> rest ()))

    member inline this.Run(m) = m

    member this.For(seq: seq<'a>, f) =
        this.Bind(
            this.Return(seq.GetEnumerator()),
            fun en -> this.While((fun () -> en.MoveNext()), this.Delay(fun () -> f en.Current))
        )

    member this.While(cond, body) =
        if not (cond ()) then
            this.Zero()
        else
            this.Combine(this.Run(body), this.Delay(fun () -> this.While(cond, body)))

[<AutoOpen>]
module StateUtils =
    let (>>=) = State.(>>=)
