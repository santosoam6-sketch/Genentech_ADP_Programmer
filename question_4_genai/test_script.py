"""
Test Script — ClinicalTrialDataAgent
=====================================
Runs 3 example queries and prints the results.

Setup:
    pip install pandas anthropic

Run with Claude (requires Anthropic API key with credits):
    export ANTHROPIC_API_KEY="sk-ant-..."
    python3 test_agent.py

Run in mock mode (no API key needed):
    python3 test_agent.py
"""

import os
from clinical_data_agent import ClinicalTrialDataAgent, load_ae_data, print_results

# ---------------------------------------------------------------------------
# 1. Load the AE dataset
# ---------------------------------------------------------------------------
df = load_ae_data("adae.csv")
print(f"Loaded adae.csv: {len(df)} rows, {df['USUBJID'].nunique()} unique subjects\n")

# ---------------------------------------------------------------------------
# 2. Initialize the agent
#    - Uses Claude if ANTHROPIC_API_KEY is set
#    - Automatically falls back to mock mode if no key is found
# ---------------------------------------------------------------------------
agent = ClinicalTrialDataAgent(
    df=df,
    api_key=os.getenv("ANTHROPIC_API_KEY"),
)

# ---------------------------------------------------------------------------
# 3. Run 3 example queries
# ---------------------------------------------------------------------------
queries = [
    # Query 1: Severity-based (LLM should map to AESEV)
    "Give me the subjects who had Adverse events of Moderate severity",

    # Query 2: Specific AE condition (LLM should map to AETERM)
    "Which patients experienced Headache?",

    # Query 3: Body system / organ class (LLM should map to AESOC)
    "Show me subjects with cardiac adverse events",
]

for question in queries:
    result = agent.ask(question)
    print_results(question, result)

print("\n" + "=" * 65)
print("  All queries completed.")
print("=" * 65)
