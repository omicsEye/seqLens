'''
This file contains the code to upload the model to the huggingface hub.
'''

import json
from transformers import AutoTokenizer, AutoConfig
import torch
from accelerate import Accelerator, DistributedType
from transformers import AutoModelForMaskedLM, set_seed, HfArgumentParser, get_scheduler, EsmConfig
from utils import get_grouped_params, setup_logging, log_metrics
from utils import compute_tflops, create_dataloaders, print_gpu_utilization
from arguments import TrainingArguments
from torch.optim import AdamW
import os
import time
from argparse import Namespace
from accelerate.utils import ProjectConfiguration
from accelerate import DistributedDataParallelKwargs


tokenizer_source = 'mahdibaghbanzadeh/seqsight_4096_512_46M'
model_ckpt='mahdibaghbanzadeh/seqsight_4096_512_46M'

tokenizer = AutoTokenizer.from_pretrained(tokenizer_source, trust_remote_code=True)

config = AutoConfig.from_pretrained(model_ckpt, trust_remote_code=True)

prefix = 'seqsight_4096_512_46M'
model_names =  [ prefix + "-" + suffix    for suffix in ['Ms', 'Me', 'Mp']]
model = AutoModelForMaskedLM.from_config(config)
for model_name in model_names:
    # push the model to the hub and make a private repo
    config.push_to_hub(model_name, use_temp_dir=True, private=True)
    model.push_to_hub(model_name, use_temp_dir=True, private=True)
    tokenizer.push_to_hub(model_name, use_temp_dir=True, private=True)
    print(f"Uploaded {model_name} to the hub")
