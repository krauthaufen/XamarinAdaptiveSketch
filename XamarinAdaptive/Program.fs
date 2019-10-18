// Learn more about F# at http://fsharp.org

open System
open Xamarin.Forms
open FSharp.Data.Adaptive
open System.Reflection

module Disposable =
    
    let empty = { new IDisposable with member x.Dispose() = () }

    let inline dispose (d : IDisposable) = d.Dispose()

[<AutoOpen>]
module Dom = 
    type AttributeValueVisitor<'r> =
        abstract member Visit : Attribute<'a, 't> * aval<'t> -> 'r

    and AttributeValue =
        abstract member Tag : Type
        abstract member Value : Type
        abstract member Name : string
        abstract member Accept : AttributeValueVisitor<'t> -> 't

    and AttributeValue<'a> =
        inherit AttributeValue

    and Attribute<'a, 'b>(name : string, set : 'a -> 'b -> IDisposable) = 
        member x.Name = name
        member x.Set(t,v) = set t v

    and AttributeAssignment<'a, 'b>(att : Attribute<'a, 'b>, value : aval<'b>) =
        interface AttributeValue with
            member x.Name = att.Name
            member x.Tag = typeof<'a>
            member x.Value = typeof<'b>
            member x.Accept(v : AttributeValueVisitor<'t>) =
                v.Visit(att, value)

        interface AttributeValue<'a>


    type Node =
        {
            tag         : Type
            create      : unit -> VisualElement
            attributes  : amap<string, AttributeValue>
            children    : alist<Node>
        }

    let (<==) (a : Attribute<'N, 'V>) (value : aval<'V>) =     
        AttributeAssignment(a,value) :> AttributeValue<_>

    let (<--) (a : Attribute<'N, 'V>) (value : 'V) =     
        AttributeAssignment(a, AVal.constant value) :> AttributeValue<_>


[<AutoOpen>]
module Attributes = 

    let inline width< ^a, ^b when ^a : (member set_Width : ^b -> unit) and ^a : (member get_Width : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_Width : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_Width : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "Width",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline height< ^a, ^b when ^a : (member set_Height : ^b -> unit) and ^a : (member get_Height : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_Height : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_Height : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "Height",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline text< ^a, ^b when ^a : (member set_Text : ^b -> unit) and ^a : (member get_Text : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_Text : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_Text : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "Text",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline textColor< ^a, ^b when ^a : (member set_TextColor : ^b -> unit) and ^a : (member get_TextColor : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_TextColor : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_TextColor : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "TextColor",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline enabled< ^a, ^b when ^a : (member set_IsEnabled : ^b -> unit) and ^a : (member get_IsEnabled : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_IsEnabled : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_IsEnabled : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "IsEnabled",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline orientation< ^a, ^b when ^a : (member set_Orientation : ^b -> unit) and ^a : (member get_Orientation : unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_Orientation : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_Orientation : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "Orientation",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )
    let inline click< ^a when ^a : (member add_Clicked : EventHandler -> unit)> =
        let add t v = (^a : (member add_Clicked : EventHandler -> unit) (t, EventHandler(fun _s e -> v e))); Disposable.empty
        Attribute< ^a, EventArgs -> unit>("Click", add)

    let inline isChecked< ^a, ^b when ^a : (member set_IsChecked : ^b -> unit) and ^a : (member get_IsChecked: unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_IsChecked : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_IsChecked : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "IsChecked",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline checkedChanged< ^a, ^e when ^a : (member add_CheckedChanged : EventHandler< ^e > -> unit)
                                      and ^a : (member remove_CheckedChanged : EventHandler< ^e > -> unit)> =

        let add (t : ^a) (cb : ^e -> unit) =
            let inline add t h = (^a : (member add_CheckedChanged : EventHandler< ^e > -> unit) (t,h))
            let inline rem t h = (^a : (member remove_CheckedChanged : EventHandler< ^e > -> unit) (t,h))

            let handler = EventHandler< ^e >(fun _s e -> cb e)
            add t handler
            { new IDisposable with member x.Dispose() = rem t handler }
        Attribute< ^a, ^e -> unit >("CheckChanged", add)


    let inline isToggled< ^a, ^b when ^a : (member set_IsToggled : ^b -> unit) and ^a : (member get_IsToggled: unit -> ^b)> : Attribute< ^a, ^b > =
        let inline set t v = ((^a) : (member set_IsToggled : ^b -> unit) (t, v))
        let inline get t = ((^a) : (member get_IsToggled : unit -> ^b) (t))
        Attribute< ^a, ^b >(
            "Toggled",
            fun t v ->
                let o = get t
                set t v
                { new IDisposable with member __.Dispose() = set t o }
        )

    let inline toggled< ^a, ^e when ^a : (member add_Toggled : EventHandler< ^e > -> unit)
                               and ^a : (member remove_Toggled : EventHandler< ^e > -> unit)> =

        let add (t : ^a) (cb : ^e -> unit) =
            let inline add t h = (^a : (member add_Toggled : EventHandler< ^e > -> unit) (t,h))
            let inline rem t h = (^a : (member remove_Toggled : EventHandler< ^e > -> unit) (t,h))

            let handler = EventHandler< ^e >(fun _s e -> cb e)
            add t handler
            { new IDisposable with member x.Dispose() = rem t handler }
        Attribute< ^a, ^e -> unit >("CheckChanged", add)


[<AutoOpen>]
module Elements =
    module Generic =
        type NodeCreator private() =
            static member Create<'a when 'a :> VisualElement> (ctor : unit -> 'a, v : amap<string, AttributeValue<'a>>, children : alist<Node>) =
                { 
                    tag = typeof<'a>
                    create = fun () -> ctor() :> VisualElement
                    attributes = v |> AMap.map (fun _ a -> a :> AttributeValue)
                    children = children
                }

            static member Create<'a when 'a :> VisualElement> (ctor : unit -> 'a, v : amap<string, AttributeValue<'a>>, children : list<Node>) =
                { 
                    tag = typeof<'a>
                    create = fun () -> ctor() :> VisualElement
                    attributes = v |> AMap.map (fun _ a -> a :> AttributeValue)
                    children = AList.ofList children
                }
            
            static member Create<'a when 'a :> VisualElement> (ctor : unit -> 'a, v : list<AttributeValue<'a>>, children : alist<Node>) =
                { 
                    tag = typeof<'a>
                    create = fun () -> ctor() :> VisualElement
                    attributes = v |> Seq.map (fun a -> a.Name, a :> AttributeValue) |> AMap.ofSeq
                    children = children
                }
            
            static member Create<'a when 'a :> VisualElement> (ctor : unit -> 'a, v : list<AttributeValue<'a>>, children : list<Node>) =
                { 
                    tag = typeof<'a>
                    create = fun () -> ctor() :> VisualElement
                    attributes = v |> Seq.map (fun a -> a.Name, a :> AttributeValue) |> AMap.ofSeq
                    children = AList.ofList children
                }

        let inline private createAux (d : ^c) ctor (atts : ^a) (cs : ^b) =
            ( (^a or ^b or ^c or ^d) : (static member Create : (unit -> ^d) * ^a * ^b -> Node) (ctor, atts, cs))

        let inline create ctor atts cs = createAux Unchecked.defaultof<NodeCreator> ctor atts cs

    let inline button v = Generic.create Button v AList.empty
    let inline checkbox v = Generic.create CheckBox v AList.empty
    let inline switch v = Generic.create Switch v AList.empty
    let inline label v = Generic.create Label v AList.empty
    let inline stack v c = Generic.create StackLayout v c


module Updaters = 

    type IAttributeUpdater =
        inherit IAdaptiveObject
        inherit IDisposable
        abstract member Update : VisualElement * AdaptiveToken -> unit

    type AttributeUpdater<'n, 'v>(att : Attribute<'n, 'v>, data : aval<'v>) =
        inherit AdaptiveObject()

        let mutable sub : Option<IDisposable> = None

        member x.PerformUpdate(element : VisualElement, token : AdaptiveToken) =    
            let element = unbox<'n> element
            let value = data.GetValue token
            let s = att.Set(element, value)
            match sub with
            | None -> sub <- Some s
            | _ -> ()

        member x.Dispose() =
            match sub with
            | Some d -> d.Dispose()
            | None -> ()
            sub <- None
            data.Outputs.Remove x |> ignore

        member x.Update(element : VisualElement, token : AdaptiveToken) =
            x.EvaluateIfNeeded token () (fun token ->
                x.PerformUpdate(element, token)
            )

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IAttributeUpdater with
            member x.Update(e,t) = x.Update(e,t)

    type ConstantAttributeUpdater<'n, 'v>(att : Attribute<'n, 'v>, data : aval<'v>) =
        inherit ConstantObject()

        let mutable sub : Option<IDisposable> = None

        member private x.Boot(element : VisualElement) =    
            let element = unbox<'n> element
            let value = data.GetValue AdaptiveToken.Top
            let s = att.Set(element, value)
            sub <- Some s

        member x.Dispose() =
            match sub with
            | Some d -> d.Dispose()
            | None -> ()
            sub <- None

        member x.Update(element : VisualElement, _token : AdaptiveToken) =
            match sub with
            | None -> x.Boot(element)
            | _ -> ()
            

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IAttributeUpdater with
            member x.Update(e,t) = x.Update(e,t)

    type NodeUpdater(node : Node, insert : VisualElement -> IDisposable) =
        inherit AdaptiveObject()

        let mutable witness = Disposable.empty
        let mutable self = None

        let reader = node.children.GetReader()
        let mutable children : IndexList<NodeUpdater> = IndexList.empty
        let dirtyInnerLock = obj()
        let mutable dirtyInner = System.Collections.Generic.HashSet<NodeUpdater>()

        let attributes = node.attributes.GetReader()
        let attributeDisp = System.Collections.Generic.Dictionary<string, IAttributeUpdater>()

        let dirtyLock = obj()
        let mutable dirty = System.Collections.Generic.HashSet<IAttributeUpdater>()

        let mutable cc : option<System.Collections.Generic.IList<View>> = None

        let mutable subNodes : IndexList<VisualElement> = IndexList.empty


        let getChildren() =
            match cc with 
            | Some c -> c
            | None -> 
                match self with
                | Some s ->
                    try
                        let props = s.GetType().GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                        let cs = 
                            props |> Array.filter (fun p -> 
                                typeof<System.Collections.Generic.IList<View>>.IsAssignableFrom p.PropertyType &&
                                p.Name = "Children"
                            )
                    
                        let l = cs.[0].GetValue(s) |> unbox
                        cc <- Some l
                        l
                    with e -> 
                        cc <- Some null
                        null
                | None ->
                    cc <- Some null
                    null


        override x.InputChangedObject(_, o : IAdaptiveObject) =
            match o with
            | :? IAttributeUpdater as u -> lock dirtyLock (fun () -> dirty.Add u |> ignore)
            | :? NodeUpdater as u -> lock dirtyInnerLock (fun () -> dirtyInner.Add u |> ignore)
            | _ -> ()

        member x.PerformUpdate(token : AdaptiveToken) =
            let dirty =
                lock dirtyLock (fun () ->
                    let o = dirty
                    dirty <- System.Collections.Generic.HashSet<IAttributeUpdater>()
                    o
                )
            let dirtyInner =
                lock dirtyInnerLock (fun () ->
                    let o = dirtyInner
                    dirtyInner <- System.Collections.Generic.HashSet<_>()
                    o
                )
            let self =
                match self with
                | None -> 
                    let e = node.create()
                    witness <- insert e
                    self <- Some e
                    e
                | Some e ->
                    e

            let attOps = attributes.GetChanges token
            for (name, op) in HashMapDelta.toSeq attOps do
                match op with
                | Set att ->
                    let sub = 
                        att.Accept { new AttributeValueVisitor<IAttributeUpdater> with
                            member x.Visit(att : Attribute<'t,'v>, value : aval<'v>) =
                                if value.IsConstant then
                                    new ConstantAttributeUpdater<_,_>(att, value) :> IAttributeUpdater
                                else    
                                    new AttributeUpdater<_,_>(att, value):> IAttributeUpdater
                        }
                    sub.Update(self, token)
                    attributeDisp.[name] <- sub
                | Remove ->
                    match attributeDisp.TryGetValue name with
                    | (true, d) -> 
                        if not d.IsConstant then dirty.Remove d |> ignore
                        attributeDisp.Remove name |> ignore
                        d.Dispose()
                    | _ ->
                        ()

            for d in dirty do
                d.Update(self, token)

            let ops = reader.GetChanges token
            for (idx, op) in IndexListDelta.toSeq ops do
                match op with
                | Set node ->
                    let cc = getChildren()
                    let insert (element : VisualElement) = 
                        let (_, s, r) = IndexList.neighbours idx subNodes

                        match s with
                        | Some(si, _) ->
                            match IndexList.tryGetPosition si subNodes with
                            | Some i -> cc.[i] <- unbox element
                            | None -> failwith "inconsistent"
                        | None ->
                            match r with
                            | Some (ri, _) ->
                                match IndexList.tryGetPosition ri children with
                                | Some i -> cc.Insert(i, unbox element)
                                | None -> failwith "inconsistent"
                            | None ->
                                // no right => last
                                cc.Add(unbox element)

                        subNodes <- IndexList.set idx element subNodes
                        { new IDisposable with
                            member x.Dispose() =   
                                match IndexList.tryGetPosition idx subNodes with
                                | Some i -> 
                                    cc.RemoveAt i
                                    subNodes <- IndexList.remove idx subNodes
                                | None -> 
                                    ()
                                
                        }

                    let u = NodeUpdater(node, insert)
                    children <- IndexList.set idx u children
                    dirtyInner.Add u |> ignore

                | Remove ->
                    match IndexList.tryRemove idx children with
                    | Some (c, rest) ->
                        dirtyInner.Remove c |> ignore
                        c.Destroy()
                        children <- rest
                    | None ->
                        ()

            for d in dirtyInner do
                d.Update(token)


        member x.Destroy() =
            witness.Dispose()

        member x.Update(token : AdaptiveToken) =
            x.EvaluateIfNeeded token () (fun token ->
                x.PerformUpdate token
            )


module Test =

    let testUI () =
        let active = cval true
        let clickCount = cval 0

        let list = clist ["booted"]

        stack [ ] [
            label [
                text <== (clickCount |> AVal.map (function 0 -> "zero" | cnt -> sprintf "clicked %d times" cnt))
                textColor <-- Color.DarkRed
            ]

            stack [ orientation <-- StackOrientation.Horizontal ] [
                switch [
                    isToggled <-- active.Value
                    toggled <-- fun e -> transact (fun () -> active.Value <- e.Value)
                ]
                label [
                    text <-- "toggle active"
                ]
            ]

            button [
                enabled <== active
                text <-- "Increment"
                click <-- fun _e -> 
                    transact (fun () -> 
                        clickCount.Value <- 1 + clickCount.Value
                        list.Add (sprintf "clicked Increment: %d" clickCount.Value) |> ignore
                    )
            ]
            button [
                enabled <== active
                text <-- "Decrement"
                click <-- fun _e -> 
                    transact (fun () -> 
                        clickCount.Value <- max 0 (clickCount.Value - 1)
                        list.Add (sprintf "clicked Decrement: %d" clickCount.Value) |> ignore
                    )
            ]
            button [
                enabled <== active
                text <-- "Reset"
                click <-- fun _e -> 
                    transact (fun () -> 
                        clickCount.Value <- 0
                        list.Value <- IndexList.ofList ["booted"]
                    )
            ]


            stack [] (
                alist {
                    for i in list do
                        yield label [ text <-- i ]
                }    
            )

        ]

    type MainWindow() = 
        inherit Xamarin.Forms.Platform.WPF.FormsApplicationPage()

    [<EntryPoint; STAThread>]
    let main _argv =
        let app = new System.Windows.Application()
        Xamarin.Forms.Forms.Init()
        let window = MainWindow() 
        window.WindowStartupLocation <- Windows.WindowStartupLocation.CenterScreen
        window.Width <- 400.0
        window.Height <- 600.0
        let page = ContentPage()
        page.Title <- "CounterApp"



        let test = testUI()
        let u = Updaters.NodeUpdater(test, fun e -> page.Content <- unbox e; Disposable.empty)

        let sub = 
            u.AddMarkingCallback(fun () -> 
                app.Dispatcher.BeginInvoke(new System.Action(fun () ->
                    u.Update(AdaptiveToken.Top)
                )) |> ignore
            )
        u.Update(AdaptiveToken.Top)


        let xapp = Application()
        xapp.MainPage <- page
        window.LoadApplication(xapp)
        let res = app.Run(window)
        sub.Dispose()
        res