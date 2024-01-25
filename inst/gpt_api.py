import os
import pandas as pd
import io
from io import StringIO
import re
import ast
import sys
# sys.path.insert(0, '/home/siwei/.local/bin')
import openai

os.chdir("/home/siwei/paper_draft_prepare/omics_multimorbidity/inst/")
# openai.api_key = "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"
# openai.api_key = "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"
shared_node = sys.argv[1]
clicked_nodes = sys.argv[2]

# response = openai.Completion.create(
#         model="gpt-3.5-turbo",
#         prompt=f"Is {shared_node} a shared biomolecules among {clicked_nodes}?",
#         temperature=0,
#         max_tokens=100,
#         frequency_penalty=0,
#         presence_penalty=0
#         )

print("This is a test, needs solving RateLimitError from openai API account...Is", str(shared_node), "a shared biomolecules among",str(clicked_nodes),"?")
