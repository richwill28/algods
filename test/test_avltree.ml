module TI = Avltree.Make(Int)

let t =
  TI.add 7
    (TI.add 6
      (TI.add 5
        (TI.add 4
          (TI.add 3
            (TI.add 2
              (TI.add 1
                (TI.add 0 TI.empty)))))))

let () = TI.iter (fun t -> Printf.printf "%i " t) t
