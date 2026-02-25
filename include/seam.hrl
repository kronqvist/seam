-ifndef(SEAM_HRL).
-define(SEAM_HRL, true).

-define(SEAM_CONDITIONS, seam_conditions).
-define(SEAM_DECISIONS,  seam_decisions).
-define(SEAM_VECTORS,    seam_mcdc_vectors).
-define(SEAM_MODULES,    seam_modules).

-type cond_key()     :: {module(), atom(), pos_integer(), pos_integer()}.
-type decision_key() :: {module(), atom(), pos_integer()}.

-endif.
