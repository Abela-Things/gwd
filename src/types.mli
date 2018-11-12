(**/**)
(** This is a fake module, used only to generate a doc templates designers. *)
(**/**)
[@@@ocaml.warning "-30"] (* Label defined in multiple types. *)

(**
   In gwd, Geneweb data are converted into Jingoo's type system.

   NB: Most of the time, fields can be undefined (i.e. represented by [null]).

   For more informations, see {{:https://geneanet.github.io/jingoo/ocaml} jingoo}
*)

type 'a pat
(** An object of type ['a]. ([Jingoo.Jg_types.Tpat]) *)

type 'a assoc
(** An [(string * 'a)] associative list. ([Jingoo.Jg_types.Tobj]) *)

type str
(** A string. ([Jingoo.Jg_types.Tstr]) *)

type bool
(** A boolean. ([Jingoo.Jg_types.Tbool]) *)

type 'a list
(** An list of ['a]. ([Jingoo.Jg_types.Tlist] or [Jingoo.Jg_types.Tarray]) *)

type int
(** An int. ([Jingoo.Jg_types.Tint]) *)

type float
(** An float. ([Jingoo.Jg_types.Tfloat]) *)

type 'a func
(** A function. ([Jingoo.Jg_types.Tfun]) *)

type 'a mandatory
(** An ['a] which can not be [null] *)

(** {2 Families and persons } *)

(** {3 Family } *)

type family =
  { are_divorced : bool
  ; are_married : bool
  ; are_engaged : bool
  ; are_not_married : bool
  ; are_separated : bool
  ; divorce_date : str
  ; children : person list
  ; father : person
  ; has_witnesses : bool
  ; ifam : int mandatory
  ; is_no_mention : bool
  ; is_no_sexes_check : bool
  ; marriage_date : date
  ; marriage_place : str
  ; marriage_note : str
  ; marriage_source : str
  ; mother : person
  ; on_marriage_date : str
  ; origin_file : str
  ; spouse : person
  ; witnesses : person list
  }
(**
   A [family] is two parents, and 0 or more children.

   A few notes about the field of [family] structure:

   - [ifam] is the [family] indentifier.
   - When the [family] is fetch from the field of a parent, [spouse] field
     is set to the other parent of this family.
     i.e. [mom.families[0].spouse = mom.families[0].father].

*)

(** {3 Person } *)

and person =
  { access : str
  ; age : dmy
  ; baptism_place : str
  ; birth_place : str
  ; burial : burial
  ; burial_place : str
  ; children : person list
  ; cop : str
  ; cremation_place : str
  ; consanguinity : float
  ; date : str
  ; dates : str
  ; death : death
  ; death_age : str
  ; death_place : str
  ; died : str
  ; digest : str
  ; events : event list
  ; families : family list
  ; father : person
  ; first_name : str
  ; first_name_aliases : str list
  ; first_name_key : str
  ; first_name_key_val : str
  ; has_children : bool
  ; has_event : bool
  ; has_image : bool
  ; has_parents : bool
  ; has_relations : bool
  ; has_siblings : bool
  ; image_url : str
  ; iper : int mandatory
  ; is_birthday : bool
  ; is_buried : bool
  ; is_certainly_dead : bool
  ; is_computable_age : bool
  ; is_computable_death_age : bool
  ; is_cremated : bool
  ; is_dead : bool
  ; is_invisible : bool
  ; is_male : bool
  ; is_female : bool
  ; is_restricted : bool
  ; linked_page : (str -> str) func
  ; max_ancestor_level : int
  ; mother : person
  ; nb_families : int
  ; nobility_titles : title list
  ; occ : int
  ; occupation : str
  ; on_baptism_date : str
  ; on_birth_date : str
  ; on_burial_date : str
  ; on_cremation_date : str
  ; on_death_date : str
  ; public_name : str
  ; qualifier : str
  ; qualifiers : str list
  ; relations : relation list
  ; related : relation list
  ; sex : int
  ; source_baptism : str
  ; source_birth : str
  ; source_burial : str
  ; source_death : str
  ; source_fsource : str
  ; source_marriage : str
  ; source_psources : str
  ; static_max_ancestor_level : int
  ; surname : str
  ; surname_aliases : str list
  ; surname_key : str
  ; surame_key_val : str
  ; title : int
  ; __str__ : str
  }

and title
(** Not implemented yet  *)

(** {3 Date/dmy } *)

and date =
  { calendar : str
  ; d2 : date
  ; day : int
  ; month : int
  ; prec : str
  ; string_of_age : str
  ; string_of_ondate : str
  ; year : int
  ;__compare__ : (date -> date -> int) func
  }
(**
   - [calendar] can be: ["Dgregorian"], ["Djulian"], ["Dfrench"] or ["Dhebrew"]
   - [prec] can be: ["sure"], ["about"], ["maybe"], ["before"], ["after"], ["oryear"] or ["yearint"]
*)

and dmy =
  { day : int
  ; delta : int
  ; month : int
  ; year : int
  }

(** {3 Death/Burial } *)

and burial
(** {b A {!type:burial} is either the string ["UnknownBurial"]
    or {!type:__burial} object.
    }
*)

and death
(** {b A {!type:death} is either one of these strings:
    ["NotDead"], ["DeadYoung"], ["DeadDontKnowWhen"], ["DontKnowIfDead"], ["OfCourseDead"],
    or {!type:__death} object.
    }
*)

and __burial =
  { type_ : str
  ; date : date
  }
(** {!recfield:__burial.type_} is actually accessible as [__burial.type] is the templates.
    (written in documentataion as [type_] because [type] is an OCaml keyword.) *)

and __death =
  { death_reason : str
  ; date: date
  }
(** [death_reason] is either
    ["Killed"], ["Murdered"], ["Executed"], ["Disappeared"] or ["Unspecified"].
*)

(** {3 Event } *)

and event =
  { date : date
  ; name : str
  ; note : str
  ; place : str
  ; spouse : person
  ; src : str
  ; witnesses : witness list
  }

and witness
(** A {!type:witness} is a {!type:person} with an extra [kind]: {!type:str} field.
*)

(** {3 Relation } *)

and relation =
  { has_relation_her : bool
  ; has_relation_him : bool
  ; related : person
  ; related_type : bool
  ; relation_type : bool
  ; relation_her : bool
  ; relation_him : bool
  }

(** {2 Conf } *)

(** Global {!type:conf} object is available in templates with the toplevel variable [conf]. *)

and conf =
  { access_by_key : bool
  ; allowed_titles : str list
  ; api_host : str
  ; api_port : int
  ; auth_file : str
  (* ; auth_scheme : auth_scheme *)
  ; authorized_wizards_notes : bool
  ; b_arg_for_basename : bool
  ; benv : str assoc
  ; bname : str
  ; border : int
  ; can_send_image : bool
  ; cgi_passwd : str
  ; charset : str
  ; command : str
  ; ctime : float
  ; default_lang : str
  ; default_sosa_ref : person
  ; denied_titles : str list
  ; env : str assoc
  ; friend : bool
  ; from : str
  ; henv : str assoc
  ; hide_names : bool
  ; highlight : str
  ; image_prefix : str
  ; indep_command : str
  ; is_printed_by_template : bool
  ; is_rtl : bool
  ; just_friend_wizard : bool
  ; lang : str
  ; left : str
  ; lexicon : lexicon pat
  ; link_to_referer : str
  ; manitou : bool
  ; multi_parents : bool
  (* ; n_connect : n_connect *)
  ; no_image : bool
  ; no_note : bool
  ; private_years : int
  ; public_if_no_date : bool
  ; public_if_titles : bool
  ; pure_xhtml : bool
  ; request : str list
  ; right : str
  ; senv : str assoc
  ; setup_link : bool
  ; supervisor : bool
  ; time : time
  ; today : dmy
  ; today_wd : int
  ; use_restrict : bool
  ; user : str
  ; username : str
  ; wizard : bool
  ; xhs : str
  }

and lexicon

and time
