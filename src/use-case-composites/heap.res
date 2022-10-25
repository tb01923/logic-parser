
module type Config = {
    type elem
    let cmp: elem => elem => bool
}

module Heap = (Config: Config) => {
    type elem = Config.elem
    let cmp = Config.cmp
    type heap = array<elem>

    module Index = {
        let left = i => (2 * i) + 1
        let right = i => (2 * i) + 2
    }

    let indexOfBest = ((h: heap), i) => {
        let heapSize = Belt.Array.length(h)

        let l = Index.left(i)
        let r = Index.right(i)

        let lValO = (l <  heapSize) ? Some(h[l]) : None
        let rValO = (r <  heapSize) ? Some(h[r]) : None
        let iVal = h[i]

        let best = switch (lValO, rValO) {
        | (Some(lVal), Some(rVal)) if cmp(lVal, iVal) && cmp(lVal, rVal) => l
        | (Some(lVal), Some(rVal)) if cmp(rVal, iVal) && cmp(rVal, lVal) => r
        | (Some(lVal), Some(rVal)) if cmp(iVal, lVal) && cmp(iVal, rVal) => i
        | (Some(lVal), None) if cmp(lVal, iVal) => l
        | (None, Some(rVal)) if cmp(rVal, iVal) => r
        | (Some(lVal), None) if cmp(iVal, lVal) => i
        | (None, Some(rVal)) if cmp(iVal, rVal) => i
        | (None, None) => i
        }

        best
    }

    let swap = ((h:heap), a, b) => {
        let  temp = h[a];
        h[a] = h[b];
        h[b] = temp;
        h
    }

    let heapify = (h: heap) => {
        let heapSize = Belt.Array.length(h)
        let rec _heapify = ((h:heap), i) =>  {

            let best = indexOfBest(h, i);

            if(best != i) {
                swap(h, i, best)
                -> _heapify(best)
            }
        }

        let start = heapSize - 1
        for i in start downto 0 {
            _heapify(h, i);
        }

        h
    }

    let make = () : heap => []

    let insert = ((h: heap), (e: elem)) : heap => {
        Belt.Array.push(h, e)->ignore
        h
    }

    let next = (h:heap) : option<'elem> => {
        heapify(h)->ignore
        let elem = Js.Array2.shift(h)
        elem
    }
}

module IntMinHeap = Heap({
  type elem = int
  let cmp = (a, b) => a < b
})


let h = IntMinHeap.make()
->IntMinHeap.insert(1)
->IntMinHeap.insert(5)
->IntMinHeap.insert(6)
->IntMinHeap.insert(8)
->IntMinHeap.insert(9)
->IntMinHeap.insert(7)
->IntMinHeap.insert(3)


let continue = ref(true)
while continue.contents {
    let next = IntMinHeap.next(h)
    switch next {
    | Some (x) => Js.Console.log(x)
    | None => continue := false
    }
}