"""
=============================================================================
Q4: GenAI Clinical Data Assistant (LLM & LangChain)
=============================================================================
Purpose  : A Generative AI Assistant that translates natural language
            questions into structured Pandas queries against an
            Adverse Events (AE) clinical dataset.
Usage:
    # With an Anthropic API key:
    agent = ClinicalTrialDataAgent(df, api_key="sk-ant-...")

    # Without an API key (mock mode):
    agent = ClinicalTrialDataAgent(df, use_mock=True)

    result = agent.ask("Show me subjects with moderate severity adverse events")
=============================================================================
"""

from __future__ import annotations

import json
import os
import re
from typing import Optional

import pandas as pd

# ---------------------------------------------------------------------------
# Optional Anthropic import
# ---------------------------------------------------------------------------
try:
    import anthropic as _anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False


# ---------------------------------------------------------------------------
# AE Dataset Schema Definition
# ---------------------------------------------------------------------------
# This dictionary is provided to Claude so it understands each column's
# clinical meaning and can map free-text questions to the correct variable.
AE_SCHEMA: dict[str, str] = {
    "STUDYID": "Study Identifier",
    "DOMAIN": "Domain Abbreviation (AE = Adverse Events)",
    "USUBJID": "Unique Subject Identifier — used to count distinct patients",
    "AESEQ": "Sequence Number within subject",
    "AETERM": (
        "Reported Term for the Adverse Event. Free-text condition name "
        "reported by the investigator (e.g., 'HEADACHE', 'NAUSEA', 'RASH'). "
        "Use this column when a user asks about a specific medical condition or symptom."
    ),
    "AESOC": (
        "Primary System Organ Class — MedDRA body-system grouping. "
        "Examples: 'Cardiac disorders', 'Nervous system disorders', "
        "'Skin and subcutaneous tissue disorders', 'Gastrointestinal disorders'. "
        "Use when user asks about a body system or organ class."
    ),
    "AEBODSYS": "Body System or Organ Class (synonym for AESOC)",
    "AESEV": (
        "Severity / Intensity of the adverse event. "
        "Coded values: MILD, MODERATE, SEVERE (always uppercase). "
        "Use when user asks about severity or intensity."
    ),
    "AESER": "Serious Event flag (Y = serious, N = non-serious)",
    "TRTEMFL": "Treatment-Emergent Flag (Y = treatment-emergent AE)",
    "ACTARM": "Actual Treatment Arm / group the subject was assigned to",
    "AESTDTC": "Start Date/Time of Adverse Event (ISO 8601 format)",
}

_AE_SCHEMA_STR = "\n".join(f"  - {col}: {desc}" for col, desc in AE_SCHEMA.items())

# ---------------------------------------------------------------------------
# System prompt sent to Claude
# ---------------------------------------------------------------------------
_SYSTEM_PROMPT = f"""You are a clinical data assistant that maps natural language questions
to structured filter queries on an Adverse Events (AE) dataset.

DATASET COLUMNS AND THEIR MEANINGS:
{_AE_SCHEMA_STR}

TASK:
Analyze the user's question and return ONLY a JSON object containing:
  - "target_column": The column to filter on (e.g., "AESEV", "AETERM", "AESOC").
  - "filter_value":  The value to search for.
                     For AESEV use uppercase: MILD | MODERATE | SEVERE.
                     For AETERM use uppercase (e.g., "HEADACHE").
                     For AESOC use Title Case (e.g., "Cardiac disorders").
  - "reasoning":     One sentence explaining your column choice.
"""


# ---------------------------------------------------------------------------
# ClinicalTrialDataAgent
# ---------------------------------------------------------------------------
class ClinicalTrialDataAgent:
    """
    An AI agent that translates natural language questions about clinical
    trial adverse-event data into structured Pandas queries.

    Logic Flow:
    1. Prompt  — builds a prompt with the dataset schema and the user's question.
    2. Parse   — sends the prompt to Claude; receives structured JSON with
                 target_column and filter_value.
    3. Execute — applies the Pandas filter and returns subject counts + IDs.

    Params:
        df       : The AE dataset (must contain USUBJID, AESEV, AETERM, AESOC).
        api_key  : Anthropic API key. Falls back to ANTHROPIC_API_KEY env var.
        use_mock : Force mock mode (keyword-based, no API call needed).
        model    : Claude model to use. Defaults to "claude-opus-4-5".
    """

    def __init__(
        self,
        df: pd.DataFrame,
        api_key: Optional[str] = None,
        use_mock: bool = False,
        model: str = "claude-opus-4-5",
    ) -> None:
        self.df = df.copy()
        self.use_mock = use_mock
        self.model = model
        self._client = None

        if not use_mock:
            resolved_key = api_key or os.getenv("ANTHROPIC_API_KEY")
            if resolved_key and ANTHROPIC_AVAILABLE:
                self._client = _anthropic.Anthropic(api_key=resolved_key)
                print(f"[AGENT] Initialized with Anthropic Claude ({self.model})")
            else:
                reason = (
                    "anthropic package not installed — run: pip install anthropic"
                    if not ANTHROPIC_AVAILABLE
                    else "ANTHROPIC_API_KEY not set"
                )
                print(f"[AGENT] {reason} — falling back to mock mode.")
                self.use_mock = True

        if self.use_mock:
            print("[AGENT] Running in MOCK mode (keyword-based LLM simulation).")

    # ------------------------------------------------------------------
    # Extract JSON from Claude's response
    # ------------------------------------------------------------------
    @staticmethod
    def _extract_json(raw: str) -> dict:
        """Parse a JSON dict from raw LLM text."""
        text = raw.strip()
        if text.startswith("```"):
            text = text.split("```")[1].lstrip("json").strip()
        return json.loads(text)

    # ------------------------------------------------------------------
    # Claude-based parsing
    # ------------------------------------------------------------------
    def _parse_with_claude(self, question: str) -> dict:
        """Send the prompt to Claude and return parsed JSON."""
        try:
            response = self._client.messages.create(
                model=self.model,
                max_tokens=256,
                system=_SYSTEM_PROMPT,
                messages=[
                    {
                        "role": "user",
                        "content": f"Question: {question}\n\nRespond with JSON only.",
                    }
                ],
            )
            return self._extract_json(response.content[0].text)
        except Exception as e:
            print(f"[AGENT] Anthropic API error: {e}")
            print("[AGENT] Falling back to mock mode for this query.")
            result = self._parse_with_mock(question)
            result["_tag"] = "MOCK LLM"
            return result

    # ------------------------------------------------------------------
    # Mock parsing (keyword-based simulation)
    # ------------------------------------------------------------------
    def _parse_with_mock(self, question: str) -> dict:
        """
        Simulate LLM parsing using keywords.
        Demonstrates the full Prompt -> Parse flow without an API call.
        """
        q = question.lower()

        # ---- 01 Severity/intensity -> AESEV ----------------------------
        severity_map = {"mild": "MILD", "moderate": "MODERATE", "severe": "SEVERE"}
        sev_triggers = {"severity", "severe", "mild", "moderate", "intensity", "grade"}

        if sev_triggers & set(q.split()):
            for kw, val in severity_map.items():
                if kw in q:
                    return {
                        "target_column": "AESEV",
                        "filter_value": val,
                        "reasoning": f"'{kw}' is a severity descriptor → AESEV = {val}.",
                    }
            return {
                "target_column": "AESEV",
                "filter_value": "MODERATE",
                "reasoning": "Severity intent detected but no level specified; defaulting to MODERATE.",
            }

        # ---- 02 Body-system/organ class -> AESOC -----------------------
        soc_keywords = [
            "cardiac", "heart", "nervous", "neurolog",
            "skin", "dermatolog", "subcutaneous",
            "gastrointestinal", "gastro", "stomach", "gut",
            "respiratory", "pulmonar", "lung",
            "musculoskeletal", "muscle", "joint",
            "blood", "haematolog", "psychiatric", "mental",
            "renal", "kidney", "hepatic", "liver",
            "endocrine", "immune", "infection",
        ]
        unique_socs = self.df["AESOC"].dropna().unique()

        for kw in soc_keywords:
            if kw in q:
                matches = [s for s in unique_socs if kw in s.lower()]
                filter_val = matches[0] if matches else kw.title()
                return {
                    "target_column": "AESOC",
                    "filter_value": filter_val,
                    "reasoning": f"Body-system keyword '{kw}' maps to AESOC.",
                }

        # ---- 03 Specific condition/term -> AETERM ----------------------
        STOP_WORDS = {
            "which", "patients", "subjects", "people", "show", "give",
            "experienced", "experiencing", "experience", "have", "with",
            "who", "had", "adverse", "events", "event", "about", "what",
            "does", "tell", "list", "find", "that", "were", "are",
            "their", "from", "there", "those", "them", "these", "this",
            "any", "all", "most", "some", "me", "the", "and", "for",
        }
        unique_terms = self.df["AETERM"].dropna().unique()
        question_words = [
            w for w in question.split()
            if len(w) > 3 and w.lower() not in STOP_WORDS
        ]

        for term in unique_terms:
            if any(w.upper() in term.upper() for w in question_words):
                return {
                    "target_column": "AETERM",
                    "filter_value": term,
                    "reasoning": f"Matched specific AE condition AETERM = '{term}'.",
                }

        # Fallback: longest meaningful word
        longest = max(question_words, key=len, default="UNKNOWN")
        longest = re.sub(r"[^\w]", "", longest).upper()
        return {
            "target_column": "AETERM",
            "filter_value": longest,
            "reasoning": f"No clear indicator; searching AETERM for '{longest}'.",
        }

    # ------------------------------------------------------------------
    # parse_question  (Prompt -> Parse)
    # ------------------------------------------------------------------
    def parse_question(self, question: str) -> dict:
        """
        Sends the user's question to Claude (or mock) and returns a
        structured dict describing which column and value to filter on.

        Params:
            question : Free-text question from the user.
        Returns:
            dict with target_column, filter_value, reasoning
        """
        print(f"\n[AGENT] Question: '{question}'")

        if self.use_mock:
            result = self._parse_with_mock(question)
            tag = "MOCK LLM"
        else:
            result = self._parse_with_claude(question)
            tag = result.pop("_tag", f"Claude ({self.model})")

        print(f"[{tag}] → column='{result['target_column']}', value='{result['filter_value']}'")
        print(f"[REASONING] {result.get('reasoning', 'N/A')}")
        return result

    # ------------------------------------------------------------------
    # execute_query  (Execute)
    # ------------------------------------------------------------------
    def execute_query(self, parsed_query: dict) -> dict:
        """
        Applies the Pandas filter derived from parsed_query and returns
        the count of unique subjects plus their IDs.

        Params:
            parsed_query : dict with target_column and filter_value.
        Returns:
            dict with count, subject_ids, total_ae_records
        """
        target_col = parsed_query["target_column"]
        filter_val = str(parsed_query["filter_value"])

        if target_col not in self.df.columns:
            return {
                "error": f"Column '{target_col}' not found in dataset.",
                "available_columns": list(self.df.columns),
            }

        col_upper = self.df[target_col].astype(str).str.strip().str.upper()
        filter_upper = filter_val.strip().upper()

        # Exact match first; substring fallback if nothing found
        matched_df = self.df[col_upper == filter_upper]
        if matched_df.empty:
            matched_df = self.df[col_upper.str.contains(filter_upper, na=False, regex=False)]

        unique_subjects = sorted(matched_df["USUBJID"].dropna().unique().tolist())

        return {
            "target_column": target_col,
            "filter_value": filter_val,
            "count": len(unique_subjects),
            "subject_ids": unique_subjects,
            "total_ae_records": len(matched_df),
        }

    # ------------------------------------------------------------------
    # ask  (Full pipeline)
    # ------------------------------------------------------------------
    def ask(self, question: str) -> dict:
        """
        Full pipeline: natural language → Claude → Pandas filter → results.

        Params:
        question : str
            question (e.g., "Give me subjects with moderate AEs").

        Returns:
        dict with query results including subject count and IDs.
        """
        parsed = self.parse_question(question)    # Stage 1: Prompt → Parse
        results = self.execute_query(parsed)      # Stage 2: Execute
        return results


# ---------------------------------------------------------------------------
# Helper Functions
# ---------------------------------------------------------------------------
def load_ae_data(filepath: str = "adae.csv") -> pd.DataFrame:
    """Load the AE dataset from a CSV file."""
    return pd.read_csv(filepath)


def print_results(question: str, results: dict) -> None:
    """Custom format print results from ClinicalTrialDataAgent.ask()."""
    border = "=" * 65
    print(f"\n{border}")
    print(f"  QUERY: {question}")
    print(border)

    if "error" in results:
        print(f"  ERROR: {results['error']}")
        return

    ids = results["subject_ids"]
    more = f"  … and {len(ids) - 10} more" if len(ids) > 10 else ""

    print(f"  Column filtered  : {results['target_column']} = '{results['filter_value']}'")
    print(f"  Matching AE rows : {results['total_ae_records']}")
    print(f"  Unique subjects  : {results['count']}")
    print(f"  Subject IDs      : {ids[:10]}{more}")
