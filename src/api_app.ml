open Geneweb
open Def
open Config
open Gwdb

module M = Api_app_piqi
module Mext = Api_app_piqi_ext

module App_date_converter = Api_util.Date_converter (M)

module NameSort =
  Set.Make
    (struct
      type t = int * string * string * string * Def.date option
      let compare (i1, sn1, fn1, _, d1) (i2, sn2, fn2, _, d2) =
        if sn1 = sn2 then
          if fn1 = fn2 then
            match (d1, d2) with
            | (Some d1, Some d2) -> Date.compare_date d1 d2
            | (Some _, None) -> -1
            | (None, Some _) -> 1
            | _ -> compare i1 i2
          else compare fn1 fn2
        else compare sn1 sn2     end)

module NameSortMap = Map.Make (String)

let intSetTab = ref (Array.make 1 0)

module IntSet =
  Set.Make
    (struct
      type t = int
      let compare x y = compare !intSetTab.(x) !intSetTab.(y)
    end)

(*
   Fichier base_info :
     - nombre de personnes
     - nombre de familles
     - sosa de référence (1-num s'il existe, 0-0 sinon)
     - timestamp de la création de la base
*)
let print_export_info conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname = Filename.concat export_directory "pb_base_info.dat" in
  match try Some (open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
    output_binary_int oc (Util.real_nb_of_persons conf base);
    output_binary_int oc (nb_of_families base);
    let sosa_ref =
      match Util.find_sosa_ref conf base with
      | Some p ->
        output_char oc '\001';
        Obj.magic @@ get_iper p
      | None ->
        output_char oc '\000';
        0
    in
    output_binary_int oc sosa_ref;
    let timestamp = string_of_float (Unix.time ()) in
    let timestamp = String.sub timestamp 0 (String.index timestamp '.') in
    output_binary_int oc (String.length timestamp);
    output_string oc timestamp;
    (* Utilisation de Extlib pour le binaire. *)
    (* let timestamp = Int32.of_float (Unix.time ()) in *)
    (* IO.write_i32 oc timestamp; *)
    close_out oc;
  | None -> ()

let pers_to_piqi_app_person conf base p =
  let gen_p = Util.string_gen_person base (gen_person_of_person p) in
  let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname = gen_p.surname in
  let occ = Int32.of_int gen_p.occ in
  let first_name = gen_p.first_name in
  let publicname = gen_p.public_name in
  let aliases = gen_p.aliases in
  let qualifiers = gen_p.qualifiers in
  let firstname_aliases = gen_p.first_names_aliases in
  let surname_aliases = gen_p.surnames_aliases in
  let image =
    if not (gen_p.image = "") then true
    else
      Hashtbl.mem Api_util.ht_img
        (Util.default_image_name_of_key
           gen_p.first_name gen_p.surname gen_p.occ)
  in
  let birth =
    match Adef.od_of_cdate gen_p.birth with
    | Some d -> Some (App_date_converter.piqi_date_of_date d)
    | _ -> None
  in
  let birth_place = gen_p.birth_place in
  let birth_src = gen_p.birth_src in
  let baptism =
    match Adef.od_of_cdate gen_p.baptism with
    | Some d -> Some (App_date_converter.piqi_date_of_date d)
    | _ -> None
  in
  let baptism_place = gen_p.baptism_place in
  let baptism_src = gen_p.baptism_src in
  let (death_type, death) =
    match gen_p.death with
    | NotDead -> (`not_dead, None)
    | Death (_, cd) ->
      let d = Adef.date_of_cdate cd in
      (`dead, Some (App_date_converter.piqi_date_of_date d))
    | DeadYoung -> (`dead_young, None)
    | DeadDontKnowWhen -> (`dead_dont_know_when, None)
    | DontKnowIfDead -> (`dont_know_if_dead, None)
    | OfCourseDead -> (`of_course_dead, None)
  in
  let death_place = gen_p.death_place in
  let death_src = gen_p.death_src in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
      (match Adef.od_of_cdate cod with
       | Some d -> Some (App_date_converter.piqi_date_of_date d)
       | _ -> None)
    | _ -> None
  in
  let burial_place = gen_p.burial_place in
  let burial_src = gen_p.burial_src in
  let occupation = gen_p.occupation in
  let psources = gen_p.psources in
  let titles =
    List.map
      (fun t ->
         let (title_type, name) =
           match t.t_name with
           | Tmain -> (`title_main, "")
           | Tname name -> (`title_name, name)
           | Tnone -> (`title_none, "")
         in
         let title = t.t_ident in
         let fief = t.t_place in
         let date_begin =
           match Adef.od_of_cdate t.t_date_start with
           | Some d -> Some (App_date_converter.piqi_date_of_date d)
           | None -> None
         in
         let date_end =
           match Adef.od_of_cdate t.t_date_end with
           | Some d -> Some (App_date_converter.piqi_date_of_date d)
           | None -> None
         in
         let nth = Some (Int32.of_int t.t_nth) in
         M.Title.({
             title_type = title_type;
             name = if name = "" then None else Some name;
             title = if title = "" then None else Some title;
             fief = if fief = "" then None else Some fief;
             date_begin = date_begin;
             date_end = date_end;
             nth = nth;
           }))
      gen_p.titles
  in
  let related = List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) gen_p.related in
  let rparents =
    List.map
      (fun rp ->
         let father =
           match rp.r_fath with
           | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
           | None -> None
         in
         let mother =
           match rp.r_moth with
           | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
           | None -> None
         in
         let source = rp.r_sources in
         let rpt_type =
           match rp.r_type with
           | Adoption -> `rpt_adoption
           | Recognition -> `rpt_recognition
           | CandidateParent -> `rpt_candidate_parent
           | GodParent -> `rpt_god_parent
           | FosterParent -> `rpt_foster_parent
         in
         M.Relation_parent.({
             father = father;
             mother = mother;
             source = if source = "" then None else Some source;
             rpt_type = rpt_type;
           }))
      gen_p.rparents
  in
  let access =
    match gen_p.access with
    | IfTitles -> `access_iftitles
    | Public -> `access_public
    | Private -> `access_private
  in
  let parents =
    match get_parents p with
    | Some ifam -> Some (Int32.of_string @@ Gwdb.string_of_ifam ifam)
    | None -> None
  in
  let families =
    List.map
      (fun i -> Int32.of_string @@ Gwdb.string_of_ifam i)
      (Array.to_list (get_family p))
  in
  let events =
    List.map
      (fun (name, date, place, note, src, w, isp) ->
         let (name, text) =
           match name with
           | Perso.Pevent name ->
             (match name with
              | Epers_Name s -> (None, Some (sou base s))
              | name -> (Some (Api_util.piqi_pevent_name_of_pevent_name name), None))
           | Perso.Fevent name ->
             (match name with
              | Efam_Name s -> (None, Some (sou base s))
              | name -> (Some (Api_util.piqi_fevent_name_of_fevent_name name), None))
         in
         let date =
           match Adef.od_of_cdate date with
           | Some d -> Some (App_date_converter.piqi_date_of_date d)
           | _ -> None
         in
         let place = sou base place in
         let note = sou base note in
         let src = sou base src in
         let witnesses =
           List.map
             (fun (ip, wk) ->
                let witness_type =
                  match wk with
                  | Witness -> `witness
                  | Witness_GodParent -> `witness_godparent
                in
                let index = Int32.of_string @@ Gwdb.string_of_iper ip in
                M.Witness_event.({
                    witness_type = witness_type;
                    witness = index;
                  }))
             (Array.to_list w)
         in
         let index_spouse =
           match isp with
           | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
           | None -> None
         in
         {
           M.Event.name = name;
           text = text;
           date = date;
           place = if place = "" then None else Some place;
           reason = None;
           note = if note = "" then None else Some note;
           src = if src= "" then None else Some src;
           witnesses = witnesses;
           index_spouse = index_spouse;
         })
      (Perso.events_list conf base p)
  in
  {
    M.Person.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    occ = occ;
    public_name = if publicname = "" then None else Some publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image = image;
    birth_date = birth;
    birth_place = if birth_place = "" then None else Some birth_place;
    birth_src = if birth_src = "" then None else Some birth_src;
    baptism_date = baptism;
    baptism_place = if baptism_place = "" then None else Some baptism_place;
    baptism_src = if baptism_src = "" then None else Some baptism_src;
    death_date = death;
    death_place = if death_place = "" then None else Some death_place;
    death_src = if death_src = "" then None else Some death_src;
    death_type = death_type;
    burial_date = burial;
    burial_place = if burial_place = "" then None else Some burial_place;
    burial_src = if burial_src = "" then None else Some burial_src;
    occupation = if occupation = "" then None else Some occupation;
    psources = if psources = "" then None else Some psources;
    titles = titles;
    related = related;
    rparents = rparents;
    access = access;
    parents = parents;
    families = families;
    events = events;
  }

let fam_to_piqi_app_family base ifam =
  let fam = foi base ifam in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let index = Int32.of_string @@ Gwdb.string_of_ifam gen_f.fam_index in
  let marriage =
    match Adef.od_of_cdate gen_f.marriage with
    | Some d -> Some (App_date_converter.piqi_date_of_date d)
    | _ -> None
  in
  let marriage_place = gen_f.marriage_place in
  let marriage_src = gen_f.marriage_src in
  let marriage_type : Api_app_piqi.marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `married
    | MarriageContract -> `married
    | MarriageLicense -> `married
    | Pacs -> `not_married
    | Residence -> `not_married
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
      (match Adef.od_of_cdate cod with
       | Some d -> (`divorced, Some (App_date_converter.piqi_date_of_date d))
       | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  let witnesses =
    List.map
      (fun i -> Int32.of_string @@ Gwdb.string_of_iper i)
      (Array.to_list gen_f.witnesses)
  in
  let fsources = gen_f.fsources in
  let father = Int32.of_string @@ Gwdb.string_of_iper ifath in
  let mother = Int32.of_string @@ Gwdb.string_of_iper imoth in
  let children =
    List.map
      (fun i -> Int32.of_string @@ Gwdb.string_of_iper i)
      (Array.to_list (get_children fam))
  in
  {
    M.Family.index = index;
    marriage_date = marriage;
    marriage_place = if marriage_place = "" then None else Some marriage_place;
    marriage_src = if marriage_src = "" then None else Some marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    witnesses = witnesses;
    fsources = if fsources = "" then None else Some fsources;
    father = father;
    mother = mother;
    children = children;
  }

(*
   Fichier person index :
     - l'adresse dans le fichier data de cette personne
   Fichier person data :
     - offset delete : à la création, c'est la fin du fichier
     - liste des taille Person, Person (proto app)
*)
let print_export_person conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in
  let fname_inx = Filename.concat export_directory "pb_base_person.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_person.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
      let curr = ref 0 in
      (* offset delete *)
      output_binary_int oc_dat 0;
      for i = 0 to nb_of_persons base - 1 do
        let ip : Gwdb.iper = Obj.magic i in
        let p = poi base ip in
        let pers_app = pers_to_piqi_app_person conf base p in
        let data = Mext.gen_person pers_app in
        let data = data `pb in
        (* Longueur de la personne puis données de la personne *)
        output_binary_int oc_dat (String.length data);
        output_string oc_dat data;
        (* Adresse de la personne *)
        output_binary_int oc_inx !curr;
        (* Attention a ne pas oublier offset delete => +4 *)
        curr := !curr + 4 + String.length data;
      done;
      (* mise à jour de offset delete maintenant qu'on a fini *)
      seek_out oc_dat 0;
      output_binary_int oc_dat !curr;
      close_out oc_dat;
      close_out oc_inx;
  | _ -> ()

(*
   Fichier family index :
    - l'adresse dans le fichier data de cette famille
   Fichier family data :
     - offset delete : à la création, c'est la fin du fichier
     - liste des taille Family, Family (proto app)
*)
let print_export_family conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in
  let fname_inx = Filename.concat export_directory "pb_base_family.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_family.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
    let curr = ref 0 in
    (* offset delete *)
    output_binary_int oc_dat 0;
    for i = 0 to nb_of_families base - 1 do
      let ifam : ifam = Obj.magic i in
      let fam_app = fam_to_piqi_app_family base ifam in
      let data = Mext.gen_family fam_app in
      let data = data `pb in
      (* Longueur de la famille puis données de la famille *)
      output_binary_int oc_dat (String.length data);
      output_string oc_dat data;
      (* Adresse de la famille *)
      output_binary_int oc_inx !curr;
      (* Attention a ne pas oublier offset delete => +4 *)
      curr := !curr + 4 + String.length data;
    done;
    (* mise à jour de offset delete maintenant qu'on a fini *)
    seek_out oc_dat 0;
    output_binary_int oc_dat !curr;
    close_out oc_dat;
    close_out oc_inx;
  | _ -> ()

(*
   Fichier person_note index :
    - l'adresse dans le fichier data de cette personne
   Fichier person_note data :
     - note vide : elle se trouve en début de fichier
     - liste des notes individuelles
*)
let print_person_note conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in
  let fname_inx = Filename.concat export_directory "pb_base_person_note.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_person_note.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
    (* Attention a ne pas oublier la note vide => +4 *)
    let curr = ref 4 in
    (* note vide *)
    output_binary_int oc_dat 0;
    for i = 0 to nb_of_persons base - 1 do
      let ip : iper = Obj.magic i in
      let p = poi base ip in
      let data = sou base (get_notes p) in
      if data = "" then
        (* On pointe vers la note vide. *)
        output_binary_int oc_inx 0
      else
        begin
          (* Adresse de la personne *)
          output_binary_int oc_inx !curr;
          output_binary_int oc_dat (String.length data);
          output_string oc_dat data;
          curr := !curr + 4 + String.length data;
        end;
    done;
    close_out oc_dat;
    close_out oc_inx;
  | _ -> ()

(*
   Fichier family_note index :
    - l'adresse dans le fichier data de cette famille
   Fichier family_note data :
     - note vide : elle se trouve en début de fichier
     - liste des notes familiales
*)
let print_family_note conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in
  let fname_inx = Filename.concat export_directory "pb_base_family_note.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_family_note.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
    (* Attention a ne pas oublier la note vide => +4 *)
    let curr = ref 4 in
    (* note vide *)
    output_binary_int oc_dat 0;
    for i = 0 to nb_of_families base - 1 do
      let ifam : ifam = Obj.magic i in
      let fam = foi base ifam in
      let data = sou base (get_comment fam) in
      if data = "" then
        (* On pointe vers la note vide. *)
        output_binary_int oc_inx 0
      else
        begin
          (* Adresse de la personne *)
          output_binary_int oc_inx !curr;
          output_binary_int oc_dat (String.length data);
          output_string oc_dat data;
          curr := !curr + 4 + String.length data;
        end;
    done;
    close_out oc_dat;
    close_out oc_inx;
  | _ -> ()

(*
   Fichier name.inx :
    -
   Fichier name.wi :
    -
   Fichier name.w :
    -
   Fichier name.i :
    -
*)
let build_relative_name base p =
  let add_from_list accu list =
    List.fold_left
      (fun accu istr ->
         if is_empty_string istr then accu
         else Name.lower (sou base istr) :: accu)
      accu list
  in
  (* Nom de jeune fille *)
  (* Plus tard, en v2
     let list =
     if get_sex p = Female then
      List.fold_left
        (fun accu ifam ->
          let fam = foi base ifam in
          let isp = Gutil.spouse (get_key_index p) fam in
          let sp = poi base isp in
          let sn = Name.lower (sou base (get_surname sp)) in
          if sn = "" then accu else sn :: accu)
        [] (Array.to_list (get_family p))
     else []
     in
  *)
  let list = [] in
  let list =
    let pn = Name.lower (sou base (get_public_name p)) in
    if pn = "" then list else pn :: list
  in
  let list = add_from_list list (get_aliases p) in
  let list = add_from_list list (get_qualifiers p) in
  let list = add_from_list list (get_first_names_aliases p) in
  let list = add_from_list list (get_surnames_aliases p) in
  List.rev list

(*
   Fichier ascends index :
    -
*)
let print_ascends_index conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in
  let fname_inx = Filename.concat export_directory "pb_base_ascends.inx" in
  match
    try Some (open_out_bin fname_inx)
    with Sys_error _ -> None
  with
  | Some oc ->
    for i = 0 to nb_of_persons base - 1 do
      let ip : iper = Obj.magic i in
      let p = poi base ip in
      match get_parents p with
      | Some ifam ->
        begin
          let cpl = foi base ifam in
          let father = get_father cpl in
          let mother = get_mother cpl in
          output_char oc '\001';
          output_binary_int oc (Obj.magic father);
          output_binary_int oc (Obj.magic mother);
        end
      | None ->
        begin
          output_char oc '\000';
          output_binary_int oc 0;
          output_binary_int oc 0;
        end
    done;
    close_out oc;
  | _ -> ()

let print_index_search conf export_directory =
  let bname = Util.base_path [] (conf.bname ^ ".gwb") in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let list_inx = ref NameSort.empty in
  let list_map = ref NameSortMap.empty in

  (* avec Hashtbl *)
  (*
  let ht = Hashtbl.create 5003 in
  let add_to_map k v =
    Hashtbl.add ht k v
  in
  *)

  (* avec Set *)
  let add_to_map k v =
    try
      let l = NameSortMap.find k !list_map in
      list_map := NameSortMap.add k (v :: l) !list_map
    with Not_found -> list_map := NameSortMap.add k [v] !list_map
  in

  let fname_inx = Filename.concat export_directory "pb_base_name.inx" in
  let fname_wi = Filename.concat export_directory "pb_base_name.wi" in
  let fname_w = Filename.concat export_directory "pb_base_name.w" in
  let fname_i = Filename.concat export_directory "pb_base_name.i" in

  begin
    try
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Obj.magic i) in
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        if sn = "?" && fn = "?" then ()
        else
          begin
            let fn = Name.lower fn in
            let sn = Name.lower sn in
            let r = String.concat " " (build_relative_name base p) in
            let date =
              match (Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p)) with
              | (Some d1, _) -> Some d1
              | (_, Some d1) -> Some d1
              | _ -> None
            in
            list_inx := NameSort.add (i, sn, fn, r, date) !list_inx;
            (* Faut il faire si y'a plusieurs espaces ? *)
            (* FIXME: Does order matter or not? If not, use List.iter instead *)
            Util.rev_iter (fun sn -> add_to_map sn i) (String.split_on_char ' ' sn);
            Util.rev_iter (fun fn -> add_to_map fn i) (String.split_on_char ' ' fn);
            Util.rev_iter (fun n -> add_to_map n i) (String.split_on_char ' ' r);
          end
      done;

      intSetTab := Array.make (nb_of_persons base) 0;
      let nb_tab = ref 0 in

      let oc_name_inx = open_out_bin fname_inx in
      List.iter
        (fun (i, _, _, _, _) ->
           output_binary_int oc_name_inx i;
           incr nb_tab;
           Array.set !intSetTab i !nb_tab)
        (NameSort.elements !list_inx);
      close_out oc_name_inx;

      let oc_name_wi = open_out_bin fname_wi in
      let oc_name_w = open_out_bin fname_w in
      let oc_name_i = open_out_bin fname_i in
      let offset_w = ref 0 in
      let offset_i = ref 0 in


      (* avec Set *)
      NameSortMap.iter
        (fun k v ->
           output_binary_int oc_name_wi !offset_w;
           output_binary_int oc_name_w (String.length k);
           output_string oc_name_w k;
           output_binary_int oc_name_w !offset_i;

           offset_w := !offset_w + 4 + (String.length k) + 4;

           (* On tri la liste pour avoir afficher les résultats triés *)
           let vv =
             IntSet.elements
               (List.fold_left
                  (fun accu i -> IntSet.add i accu)
                  IntSet.empty v)
           in
           output_binary_int oc_name_i (List.length vv);
           List.iter (output_binary_int oc_name_i) vv;

           offset_i := !offset_i + 4 + (4 * (List.length vv)))
        !list_map;

      (* avec Hashtbl *)
      (*
      let last_key = ref "" in
      let vv = ref IntSet.empty in
      let len = Hashtbl.length ht in
      let i = ref 0 in
      Hashtbl.iter
        (fun k v ->
          incr i;
          (* On tri la liste pour avoir afficher les résultats triés *)
          if k = !last_key then vv := IntSet.add v !vv else ();
          if k <> !last_key || len = !i then
            begin
              output_binary_int oc_name_wi !offset_w;
              output_binary_int oc_name_w (String.length !last_key);
              output_string oc_name_w !last_key;
              output_binary_int oc_name_w !offset_i;
              offset_w := !offset_w + 4 + (String.length !last_key) + 4;
              output_binary_int oc_name_i (IntSet.cardinal !vv);
              IntSet.iter (output_binary_int oc_name_i) !vv;
              offset_i := !offset_i + 4 + (4 * (IntSet.cardinal !vv));
              vv := IntSet.empty;
            end)
        ht;
      *)

      close_out oc_name_wi;
      close_out oc_name_w;
      close_out oc_name_i;

    with Sys_error _ -> ()
  end

let print_export conf base =
  let () = load_ascends_array base in
  let () = load_strings_array base in
  let () = load_couples_array base in
  let () = load_unions_array base in
  let () = load_descends_array base in
  let () = Api_util.load_image_ht conf in
  (*
     On créé X processus pour l'export :
       - 1/ info
       - 2/ person
       - 3/ family
       - 4/ search
       - 5/ person note
       - 6/ family note
       - 7/ ascends index
  *)

  let export_directory =
    match Api_util.p_getenvbin conf.env "data" with
    | Some s -> s
    | None -> exit 2
  in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  let process =
    [ print_index_search; print_export_person; print_export_family;
      print_person_note; print_family_note; print_ascends_index;
      print_export_info ]
  in

  let nb_process = List.length process in

  List.iter
    (fun f ->
       match Unix.fork () with
       | 0 ->
         (* children code *)
         begin
           f conf tmp_export_directory;
           exit 0
         end
       | -1 -> failwith "fork error"
       | _ -> ())
    process;

  (* wait for all children *)
  for _ = 0 to nb_process - 1 do ignore (Unix.wait()) done;

  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in

  Util.html conf


(**/**) (* Version app, synchro !!! *)


module IntIdSet = Set.Make (struct type t = int let compare = compare end)

(* synchro_patch stuff is copy/paste from gwd1/database.ml *)

type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }

let input_synchro bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "synchro_patches") in
    let r : synchro_patch = input_value ic in
    close_in ic ;
    r
  with _ -> {synch_list = []}

let full_synchro conf synchro timestamp =
  let last_import = ref None in
  let bdir =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  (* Suppression potentiel du fichier patch. *)
  (match synchro.synch_list with
   | (last_timestamp, _, _) :: _ ->
     let fname_synchro = Filename.concat bdir "synchro_patches" in
     let fname_cmd = Filename.concat bdir "command.txt" in
     (match
        try Some (open_in (Util.base_path [] fname_cmd))
        with Sys_error _ -> None
      with
      | Some ic ->
        let fd = Unix.descr_of_in_channel ic in
        let stats = Unix.fstat fd in
        close_in ic;
        last_import := Some stats.Unix.st_mtime;
        if float_of_string last_timestamp < stats.Unix.st_mtime then
          try Sys.remove fname_synchro with Sys_error _ -> ()
        else ()
      | None -> ());
   | _ -> ());
  (* On clean le fichier synchro des trop vieilles modifs. *)
  (match !last_import with
   | Some last_mod ->
     let bname = Util.base_path [] bdir in
     let new_synchro = input_synchro bname in
     let list =
       List.fold_right
         (fun (ts, ipl, ifaml) accu ->
            if (float_of_string ts < last_mod) then accu
            else (ts, ipl, ifaml) :: accu)
         new_synchro.synch_list []
     in
     let new_synchro = {synch_list = list} in
     (* Si on a rien modifier, ça ne sert à rien de faire la mise à *)
     (* jour du fichier synchro, parce qu'on modifie la date de     *)
     (* dernière modification pour rien.                            *)
     if synchro = new_synchro then ()
     else
       begin
         let tmp_fname = Filename.concat bname "1synchro_patches" in
         let fname = Filename.concat bname "synchro_patches" in
         let oc9 =
           try Secure.open_out_bin tmp_fname
           with Sys_error _ ->
             raise (Adef.Request_failure "the database is not writable")
         in
         Mutil.output_value_no_sharing oc9 (synchro : synchro_patch);
         close_out oc9;
         Mutil.remove_file (fname ^ "~");
         (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
         (try Sys.rename tmp_fname fname with Sys_error _ -> ());
       end
   | _ -> ());
  (* Si timestamp plus petit que import, alors synchro totale. *)
  match !last_import with
  | Some last_mod -> if timestamp < last_mod then true else false
  | _ -> false

let print_synchro_patch_mobile conf base =
  let params = Api_util.get_params conf Mext.parse_synchro_params in
  let export_directory = params.M.Synchro_params.export_directory in
  let timestamp = params.M.Synchro_params.timestamp in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  (* Récupération du fichier synchro. *)
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  let bname = Util.base_path [] bname in
  let synchro = input_synchro bname in
  (* Toutes les dernières modifications. *)
  let timestamp = float_of_string timestamp in
  let (ip_list, ifam_list) =
    List.fold_right
      (fun (t_stamp, ip_list, ifam_list) (accu_ip_list, accu_ifam_list) ->
         let t_stamp = float_of_string t_stamp in
         if t_stamp > timestamp then
           (accu_ip_list @ ip_list, accu_ifam_list @ ifam_list)
         else (accu_ip_list, accu_ifam_list))
      synchro.synch_list ([], [])
  in
  let last_timestamp =
    match synchro.synch_list with
    | (timestamp, _, _) :: _ -> timestamp
    | _ -> ""
  in
  (* On rend unique les ids. *)
  let ip_list =
    IntIdSet.elements
      (List.fold_left
         (fun accu i -> IntIdSet.add i accu)
         IntIdSet.empty ip_list)
  in
  let ifam_list =
    IntIdSet.elements
      (List.fold_left
         (fun accu i -> IntIdSet.add i accu)
         IntIdSet.empty ifam_list)
  in
  let len_ip_list = List.length ip_list in
  let len_ifam_list = List.length ifam_list in

  (* Ecriture du fichier synchro. *)
  let fname = Filename.concat tmp_export_directory "pb_base_synchro.patches" in
  let () =
    match
      try Some (open_out_bin fname)
      with Sys_error _ -> None
    with
    | Some oc ->
      if full_synchro conf synchro timestamp then
        (* si 0 il faut re-synchroniser la base. *)
        output_char oc '\000'
      else
        begin
          (* si 1 il faut appliquer le patch. *)
          output_char oc '\001';
          output_binary_int oc (String.length last_timestamp);
          output_string oc last_timestamp;
          (* nb persons et families *)
          output_binary_int oc (Util.real_nb_of_persons conf base);
          output_binary_int oc (nb_of_families base);
          (* sosa *)
          let sosa_ref =
            match Util.find_sosa_ref conf base with
            | Some p ->
              output_char oc '\001';
              Obj.magic (get_iper p)
            | None ->
              output_char oc '\000';
              0
          in
          output_binary_int oc sosa_ref;
          (* nb pers modified, id len pers *)
          output_binary_int oc len_ip_list;
          List.iter
            (fun i ->
               let ip = Obj.magic i in
               let p = poi base ip in
               let pers_app = pers_to_piqi_app_person conf base p in
               let data = Mext.gen_person pers_app in
               let data = data `pb in
               (* id, longueur de la personne puis données de la personne *)
               output_binary_int oc i;
               output_binary_int oc (String.length data);
               output_string oc data)
            ip_list;
          (* nb fam modified, id len fam *)
          output_binary_int oc len_ifam_list;
          List.iter
            (fun i ->
               let ifam = Gwdb.ifam_of_string @@ string_of_int i in
               let fam_app = fam_to_piqi_app_family base ifam in
               let data = Mext.gen_family fam_app in
               let data = data `pb in
               (* id, longueur de la famille puis données de la famille *)
               output_binary_int oc i;
               output_binary_int oc (String.length data);
               output_string oc data)
            ifam_list;
          (* nb pers modified, id len pers_note *)
          output_binary_int oc len_ip_list;
          List.iter
            (fun i ->
               let ip = Obj.magic i in
               let p = poi base ip in
               let data = sou base (get_notes p) in
               if data = "" then
                 begin
                   (* On pointe vers la note vide. *)
                   output_binary_int oc i;
                   output_binary_int oc 0
                 end
               else
                 begin
                   (* id, longueur de la personne puis données de la personne *)
                   output_binary_int oc i;
                   output_binary_int oc (String.length data);
                   output_string oc data
                 end)
            ip_list;
          (* nb fam modified, id len fam_note *)
          output_binary_int oc len_ifam_list;
          List.iter
            (fun i ->
               let ifam = Gwdb.ifam_of_string @@ string_of_int i in
               let fam = foi base ifam in
               let data = sou base (get_comment fam) in
               if data = "" then
                 begin
                   (* On pointe vers la note vide. *)
                   output_binary_int oc i;
                   output_binary_int oc 0
                 end
               else
                 begin
                   (* id, longueur de la personne puis données de la personne *)
                   output_binary_int oc i;
                   output_binary_int oc (String.length data);
                   output_string oc data
                 end)
            ifam_list;
          (* nb pers modified, id has_parents id_father id_mother *)
          output_binary_int oc len_ip_list;
          List.iter
            (fun i ->
               let ip = Obj.magic i in
               let p = poi base ip in
               output_binary_int oc i;
               match get_parents p with
               | Some ifam ->
                 begin
                   let cpl = foi base ifam in
                   let father = get_father cpl in
                   let mother = get_mother cpl in
                   output_char oc '\001';
                   output_binary_int oc (Obj.magic father);
                   output_binary_int oc (Obj.magic mother);
                 end
               | None ->
                 begin
                   output_char oc '\000';
                   output_binary_int oc 0;
                   output_binary_int oc 0;
                 end)
            ip_list;
          (* number of character => to be modified
             when we actually know the number. *)
          let nb_char = ref 0 in
          let pos_nb_char = pos_out oc in
          output_binary_int oc !nb_char;
          (* id nb_word len_word word *)
          List.iter
            (fun i ->
               let ip = Obj.magic i in
               let p = poi base ip in
               let fn = sou base (get_first_name p) in
               let sn = sou base (get_surname p) in
               if sn = "?" && fn = "?" then ()
               else
                 begin
                   let fn = Name.lower fn in
                   let sn = Name.lower sn in
                   let r = build_relative_name base p in
                   output_binary_int oc i;
                   let (split_l, nb_words, nb_chars) =
                     List.fold_left
                       (fun (split_l, nb_words, nb_chars) s ->
                          (* FIXME: Does order matter or not? *)
                          let l = String.split_on_char ' ' s |> List.rev in
                          let sub_nb_chars =
                            match List.length l with
                            | 0 -> 0
                            | x -> String.length s - (x - 1)
                          in
                          (List.rev_append split_l l,
                           nb_words + List.length l,
                           nb_chars + sub_nb_chars))
                       ([], 0, 0) (sn :: fn :: r)
                   in
                   nb_char := 4 + 4 + (4 * nb_words) + nb_chars + !nb_char;
                   output_binary_int oc nb_words;
                   List.iter
                     (fun s ->
                        output_binary_int oc (String.length s);
                        output_string oc s)
                     split_l
                 end)
            ip_list;
          (* update nb_char *)
          seek_out oc pos_nb_char;
          output_binary_int oc !nb_char;
        end;
      close_out oc;
    | _ -> exit 2
  in

  (* move file *)
  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in

  Util.html conf

let print_export_search conf base =
  let () = load_strings_array base in

  let export_directory =
    match Api_util.p_getenvbin conf.env "data" with
    | Some s -> s
    | None -> exit 2
  in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  (* Génération des fichiers de recherche. *)
  print_index_search conf tmp_export_directory;

  (* move file *)
  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in

  Util.html conf
