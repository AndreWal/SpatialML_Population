# targets pipeline rules
- Every material processing step should be a target.
- Prefer dynamic branching by country.
- Avoid reading/writing outside target steps.
- Target names should reflect stage + entity (e.g., `harmonized_tabular_country`).
- Do not disable checks to force a green run.
