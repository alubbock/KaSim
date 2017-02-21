type rate_convention =
  | KaSim
  | Divide_by_nbr_of_autos_in_lhs
  | Biochemist

type count = Embeddings | Occurrences

type t = {
  mutable backend : string ;
  mutable rate_convention : string ;
  mutable count : string ;
  mutable show_reactions : bool ;
  mutable compute_jacobian : bool ;
  mutable octave_output : string option ;
  mutable matlab_output : string option ;
  mutable sbml_output : string option ;
  mutable with_symmetries : bool ;
  mutable views : bool ;
  mutable dbonds : bool ;
  mutable site_accross : bool ;
}

let default : t =
  {
    backend = "Octave" ;
    rate_convention = "Divide_by_nbr_of_autos_in_lhs" ;
    count = "Embeddings" ;
    show_reactions = true ;
    compute_jacobian = false ;
    octave_output = None  ;
    matlab_output = None ;
    sbml_output = None ;
    with_symmetries = false ;
    views = true ;
    dbonds = true ;
    site_accross = true ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  "--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
   "Available backends are Octave and Matlab" ;
  "--matlab-output",
  Arg.String (fun backend -> t.matlab_output <- Some backend),
  "ODEs file for matlab backend";
  "--octave-output",
  Arg.String (fun backend -> t.octave_output <- Some backend),
  "ODEs file for octave backend";
  "--sbml-output",
  Arg.String (fun backend -> t.sbml_output <- Some backend),
  "ODEs file for sbml backend";
  "--rate-convention",
  Arg.String (fun rate_convention -> t.rate_convention <- rate_convention),
  "Tune which correction is applied to rule rates \n\t KaSim -> we do not divide; \n\t Divide_by_nbr_of_autos_in_lhs -> we divide by the number of autos in the lhs \n\t Biochemist -> we divide by the number of autos in the lhs that induce an auto in the rhs" ;
  "--count",
  Arg.String (fun count -> t.count <- count),
  "Tune whether ode variables denote number of occurrences or number of embeddings" ;
  "--show-reactions",
  Arg.Bool (fun show_reactions -> t.show_reactions <- show_reactions),
  "Annotate ODEs by the corresponding chemical reactions" ;
  "--compute-jacobian",
  Arg.Bool (fun compute_jacobian -> t.compute_jacobian <- compute_jacobian),
  "Enable/disable the computation of the Jacobian of the ODEs \n\t (not available yet)" ;
  "--with-symmetries",
  Arg.Bool (fun with_symmetries -> t.with_symmetries <- with_symmetries),
  "Enable/disable the quotient of the set of species up to permutation of symmetric sites" ;
  "--views-domain",
  Arg.Bool (fun views -> t.views <- views),
  "Enable/disable views analysis when detecting symmetric sites" ;
  "--double-bonds-domain",
  Arg.Bool (fun dbonds -> t.dbonds <- dbonds),
  "Enable/disable double bonds analysis when detecting symmetric sites" ;
  "--site-accross-bonds-domain",
  Arg.Bool (fun site_accross -> t.site_accross <- site_accross),
  "Enable/disable the analysis of the relation amond the states of sites in
      connected agents";
]

let build_kasa_parameters t t_common =
  Config.with_views_analysis := t.views ;
  Config.with_parallel_bonds_analysis := t.dbonds ;
  Config.with_site_accross_bonds_analysis := t.site_accross ;
  Config.trace := t_common.Common_args.debug ;
  Remanent_parameters.get_parameters
    ~called_from:Remanent_parameters_sig.Server
    ()
