###########################################################################
## Script to export single state-year returns of municipal races.        ##
## Invoked from extract-state-yr-mu-returns.py in                        ##
## https://github.com/emagar/elecRetrns.git                              ##
##                                                                       ##
## Author: Eric Magar emagar at itam dot mx                              ##
## Translated to Python with chatGPT from original R code on: 6-jun-2026 ##
## Last revised: 6-jun-2026                                              ##
###########################################################################

import os
from pathlib import Path

import pandas as pd
import requests

def xport(e=None, y=None, coal_agg=True, write_to_file=False):
    """
    Export municipal vote returns for a given state and year.

    Parameters
    ----------
    e : int
        State number (1-32).
    y : int
        Election year.
    coal_agg : bool, default=True
        Use coalition aggregates if True, split coalitions otherwise.
    write_to_file : bool, default=False
        If True, write output CSV to disk.
    """

    edos = [
        "ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua",
        "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic",
        "mor", "nay", "nl", "oax", "pue", "que", "qui", "san",
        "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"
    ]

    estados = [
        "Aguascalientes", "Baja California", "Baja California Sur",
        "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
        "Distrito Federal/CDMX", "Durango", "Guanajuato", "Guerrero",
        "Hidalgo", "Jalisco", "México (Estado de)", "Michoacán",
        "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
        "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
        "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
        "Yucatán", "Zacatecas"
    ]

    file_name = (
        "aymu1970-on.coalAgg.csv"
        if coal_agg
        else "aymu1970-on.coalSplit.csv"
    )

    url = (
        "https://raw.githubusercontent.com/emagar/"
        "elecRetrns/refs/heads/master/data/"
        f"{file_name}"
    )

    if not Path(file_name).exists():
        response = requests.get(url)
        response.raise_for_status()

        with open(file_name, "wb") as f:
            f.write(response.content)

    dat = pd.read_csv(file_name)

    # subset to requested state/year
    dat = dat[(dat["edon"] == e) & (dat["yr"] == y)].copy()

    if dat.empty:
        raise ValueError(
            "No rows in data. Did you select a valid electoral year?"
        )

    # vote columns (v01, v02, ...)
    vote_cols = [
        c for c in dat.columns
        if c.startswith("v") and len(c) == 3 and c[1:].isdigit()
    ]

    # label columns (l01, l02, ...)
    label_cols = [
        c for c in dat.columns
        if c.startswith("l") and len(c) == 3 and c[1:].isdigit()
    ]

    v = dat[vote_cols].copy()
    l = dat[label_cols].copy()

    # replace "-" with "." in labels
    l = l.replace("-", ".", regex=True)

    # all unique labels
    cols = pd.unique(l.to_numpy().ravel())
    cols = [c for c in cols if pd.notna(c) and str(c) != "0"]

    # initialize output vote matrix
    vl = pd.DataFrame(
        0,
        index=dat.index,
        columns=cols
    )

    # populate party/coalition columns
    for party in cols:

        def get_vote(row_idx):
            labels = l.loc[row_idx]

            matches = [
                i for i, val in enumerate(labels)
                if val == party
            ]

            if not matches:
                return 0

            return v.iloc[row_idx, matches[0]]

        vl[party] = [
            get_vote(i)
            for i in range(len(dat))
        ]

    # locate first and last vote/label columns
    vl_cols = [
        i for i, c in enumerate(dat.columns)
        if (
            (c.startswith("v") or c.startswith("l"))
            and len(c) == 3
            and c[1:].isdigit()
        )
    ]

    first_vl = min(vl_cols)
    last_vl = max(vl_cols)

    dat1 = dat.iloc[:, :first_vl]
    dat2 = dat.iloc[:, last_vl + 1:]

    export = pd.concat(
        [dat1.reset_index(drop=True),
         vl.reset_index(drop=True),
         dat2.reset_index(drop=True)],
        axis=1
    )

    if not write_to_file:
        return export

    os.makedirs("xport", exist_ok=True)

    outfile = f"xport/{edos[e - 1]}{y}aymu.csv"

    export.to_csv(outfile, index=False)

    return (
        f"{y} municipal vote returns for "
        f"{estados[e - 1]} exported as {outfile}"
    )
