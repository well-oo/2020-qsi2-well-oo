open Framework;
open QCheckRely;
open Generator.Fantasy;
open Lib.Troll;

let {describe} = extendDescribe(QCheckRely.Matchers.matchers);

describe("Troll Invariance", ({test}) => {
  test("Troll score should be 0 when all elves resurrected", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should be 0 when all elves resurrected",
      troll_arbitrary,
      troll =>
      all_elves_resurrected(troll) |> scoring == 0
    )
    |> expect.ext.qCheckTest;
    ();
  });
  test("Troll score should always be >= 0", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should always be >= 0",
      troll_arbitrary,
      troll => scoring(troll) >= 0
    )
    |> expect.ext.qCheckTest;
    ();
  });
});

describe("Troll Inverse", ({test}) => {
  test("oops_he_survived should always be inverse of i_got_one", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="oops_he_survived should always be inverse of i_got_one",
      troll_elf_arbitrary,
      ((troll, elfe)) => {
        scoring(troll |> i_got_one(elfe) |> oops_he_survived(elfe)) == scoring(troll)
      }
    )
    |> expect.ext.qCheckTest;
    ();
  });
});

describe("Troll Analogy", ({test}) => {
  test("i_got_one and i_got should be consistent", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="i_got_one and i_got should be consistent",
      troll_elf_int_arbitrary,
      ((troll, elfe, qty)) => {
        /*
        Avec un for
        let trolls = ref(troll);
        for(i in 1 to qty){
          trolls := i_got_one(elfe, trolls^);
        }
        scoring(i_got(qty, elfe, troll)) == scoring(trolls^)
        */
        (i_got(qty, elfe, troll) |> scoring) == (List.fold_left(
                                                  (t,_) => i_got_one(elfe, t),
                                                  troll,
                                                  List.init(qty, _ => "aya")
                                                ) |> scoring)
      }
    )
    |> expect.ext.qCheckTest;
    ();
  });
});

describe("Troll Idempotence", ({test}) => {
  test(
    "all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
        troll_elf_int_arbitrary,
        ((troll, elfe, qty)) => {
          all_elves_of_a_kind_resurrected(elfe, troll) == ( all_elves_of_a_kind_resurrected(elfe, troll) |> 
                                                            List.fold_right(
                                                              (_,t) => all_elves_of_a_kind_resurrected(elfe,t),
                                                              List.init(qty, _ => "aya")
                                                            )
                                                          )
        }
      )
      |> expect.ext.qCheckTest;
      ();
  });
});

describe("Troll Metamorphism", ({test}) => {
  test(
    "i_got_one should increment Troll's score",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="i_got_one should increment Troll's score",
        troll_elf_arbitrary,
        ((troll, elfe)) => {
          scoring(troll) < (i_got_one(elfe, troll) |> scoring)
        }
      )
      |> expect.ext.qCheckTest;
      ();
  });
});

describe("Troll Injection", ({test}) => {
  test(
    "i_got_one update the troll in a unique way",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="i_got_one update the troll in a unique way",
        troll_two_elves_arbitrary,
        ((troll, elfe1, elfe2)) => {
          (elfe1 != elfe2) ? i_got_one(elfe1, troll) != i_got_one(elfe2, troll) : i_got_one(elfe1, troll) == i_got_one(elfe2, troll)
        }
      )
      |> expect.ext.qCheckTest;
      ();
  });
});