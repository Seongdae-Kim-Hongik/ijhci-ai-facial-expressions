"""Low-level image properties of the fourteen stimuli.

Pupil diameter responds strongly to display luminance, so a source difference in
luminance that varies by emotion category would reproduce the Emotion x Source
interaction observed in the pupil data without any affective process. This
script measures the properties that matter and writes them for the supplement.

Usage:
    python3 python/08_stimulus_properties.py <stimulus_dir> <output_csv>
"""
import csv
import re
import sys
import unicodedata
from pathlib import Path

import numpy as np
from PIL import Image

# The eye-tracker's media names encode the intended category, in Korean for the
# AI-generated set and in English for the human set.
CATEGORY = {
    "anger": ("anger", "분노"), "contempt": ("contempt", "경멸"),
    "disgust": ("disgust", "혐오"), "fear": ("fear", "두려움"),
    "happiness": ("happiness", "기쁨"), "sadness": ("sadness", "슬픔"),
    "surprise": ("surprise", "놀람"),
}


def classify(name):
    # macOS stores Korean filenames in decomposed form, which will not match a
    # composed literal, so both sides are normalised before comparison.
    stem = unicodedata.normalize("NFC", name).lower()
    source = "AI" if re.search(r"\bai\b|ai\.", stem) else "Human"
    for canonical, tokens in CATEGORY.items():
        if any(unicodedata.normalize("NFC", t).lower() in stem for t in tokens):
            return canonical, source
    return None, source


def srgb_to_linear(x):
    x = x / 255.0
    return np.where(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055) ** 2.4)


def properties(path):
    img = Image.open(path).convert("RGB")
    width, height = img.size
    rgb = np.asarray(img, dtype=np.float64)

    # CIE relative luminance on linearised sRGB, which is what the eye receives.
    lin = srgb_to_linear(rgb)
    Y = 0.2126 * lin[..., 0] + 0.7152 * lin[..., 1] + 0.0722 * lin[..., 2]

    mean_Y = Y.mean()
    rms_contrast = Y.std() / mean_Y if mean_Y > 0 else np.nan
    p_low, p_high = np.percentile(Y, [1, 99])
    michelson = (p_high - p_low) / (p_high + p_low) if (p_high + p_low) > 0 else np.nan

    # Perceived lightness, and colourfulness as a secondary descriptor.
    grey = np.asarray(img.convert("L"), dtype=np.float64)
    hsv = np.asarray(img.convert("HSV"), dtype=np.float64)

    # High spatial frequency energy, as a proxy for edge and texture density.
    small = np.asarray(Image.fromarray(grey.astype(np.uint8)).resize(
        (256, 256), Image.LANCZOS), dtype=np.float64)
    gy, gx = np.gradient(small)
    edge_energy = float(np.sqrt(gx ** 2 + gy ** 2).mean())

    return {
        "width": width,
        "height": height,
        "mean_luminance": round(float(mean_Y), 5),
        "rms_contrast": round(float(rms_contrast), 4),
        "michelson_contrast": round(float(michelson), 4),
        "mean_grey_level": round(float(grey.mean()), 2),
        "sd_grey_level": round(float(grey.std()), 2),
        "mean_saturation": round(float(hsv[..., 1].mean()), 2),
        "edge_energy": round(edge_energy, 4),
    }


def main(stimulus_dir, output_csv):
    rows = []
    for path in sorted(Path(stimulus_dir).iterdir()):
        if path.suffix.lower() not in {".jpg", ".jpeg", ".png"}:
            continue
        emotion, source = classify(path.name)
        if emotion is None:
            continue
        rows.append({"file": path.name, "emotion": emotion, "source": source,
                     **properties(path)})

    rows.sort(key=lambda r: (r["emotion"], r["source"]))
    if not rows:
        raise SystemExit("No stimuli found in %s" % stimulus_dir)

    with open(output_csv, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

    print("%d stimuli measured -> %s" % (len(rows), output_csv))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
