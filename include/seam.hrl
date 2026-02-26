-ifndef(SEAM_HRL).
-define(SEAM_HRL, true).

-define(SEAM_CONDITIONS, seam_conditions).
-define(SEAM_DECISIONS,  seam_decisions).
-define(SEAM_VECTORS,    seam_mcdc_vectors).
-define(SEAM_MODULES,    seam_modules).
-define(SEAM_META,       seam_meta).
-define(SEAM_DISCOVERIES, seam_discoveries).
-define(SEAM_OPERANDS,   seam_operands).
-define(SEAM_EDGES,      seam_edges).
-define(SEAM_DECISION_META, seam_decision_meta).

-type cond_key()     :: {module(), atom(), pos_integer(), pos_integer()}.
-type decision_key() :: {module(), atom(), pos_integer()}.
-type discovery()    :: {condition, cond_key(), boolean()}
                      | {decision, decision_key(), boolean()}
                      | {edge, decision_key(), decision_key()}.

-endif.
