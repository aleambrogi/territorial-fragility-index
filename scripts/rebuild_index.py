"""Reproduce final aggregation from saved, rank-normalised macro-area scores."""
from pathlib import Path
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
COLS = ["indice_ambientale", "indice_socio", "indice_economico", "indice_servizi"]

def aggregate(frame):
    if frame.pro_com.duplicated().any():
        raise ValueError("Duplicate municipality codes")
    x = frame[COLS].to_numpy(dtype=float)
    if np.isinf(x).any() or (x < 0).any() or (x > 1).any():
        raise ValueError("Expected rank-normalised scores in [0, 1]")
    distance = np.sqrt(np.sum(x ** 2, axis=1))
    span = np.nanmax(distance) - np.nanmin(distance)
    if span == 0:
        raise ValueError("A constant distance cannot be min-max normalised")
    return pd.DataFrame({"pro_com": frame.pro_com, "indice_finale": (distance-np.nanmin(distance))/span})

def main():
    out = ROOT / "results/generated"
    out.mkdir(parents=True, exist_ok=True)
    summary = {}
    for year in (2019, 2021):
        components = pd.read_csv(ROOT/f"data/processed/components_{year}.csv")
        actual = aggregate(components)
        expected = pd.read_csv(ROOT/f"data/processed/reference_index_{year}.csv")
        matched = actual.merge(expected, on="pro_com", validate="one_to_one", suffixes=("", "_reference"))
        if len(matched) != len(actual) or len(expected) != len(actual):
            raise ValueError("Reference municipality coverage differs")
        if not matched.indice_finale.isna().equals(matched.indice_finale_reference.isna()):
            raise ValueError("Missingness differs from reference")
        error = float((matched.indice_finale-matched.indice_finale_reference).abs().max())
        if error > 1e-12:
            raise ValueError(f"Reference mismatch: {error}")
        actual.to_csv(out/f"index_{year}.csv", index=False)
        summary[year] = {"municipalities": len(actual), "missing_final_scores": int(actual.indice_finale.isna().sum()), "max_absolute_reference_error": error,
                         "minimum": float(actual.indice_finale.min()), "maximum": float(actual.indice_finale.max())}
    (out/"verification.json").write_text(json.dumps(summary, indent=2)+"\n")
    print(json.dumps(summary, indent=2))

if __name__ == "__main__":
    main()
