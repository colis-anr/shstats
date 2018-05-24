module Self : Analyzer.S = struct
  let name = "identity"
  let options = []

  let process_script filename csts =
    ()

  let output_report () =
    ()
end

let install = Analyzer.register (module Self)
