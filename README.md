# seqLens

This repository contains the code for the [`seqLens` manuscript](https://academic.oup.com/mbe/article/43/7/msag139/8715817). In the `seqLens` project, we investigated genomics language models by implementing a set of DNA language models and benchmarked their efficiency.  
Hugging Face models: [omicseye/seqLens](https://huggingface.co/collections/omicseye/seqlens-67d1ff371d28e9ef2cacba63)

`seqLens` manuscript: [https://doi.org/10.1093/molbev/msag139](https://doi.org/10.1093/molbev/msag139)
## Overview
![](https://github.com/omicsEye/seqLens/blob/main/visualizations/plots/png/fig1_wide.png)

Our research introduces several key innovations in DNA sequence modeling. We gathered two different pre-training datasets consisting of:
- 19,551 reference genomes with over 18,000 prokaryote genomes (over 115B nucleotides)
- A more balanced dataset composed of 1,355 prokaryote and eukaryote reference genomes (over 180B nucleotides)

We trained five different byte-pair encoding tokenizers and pre-trained 52 DNA language models. We introduce `seqLens` models, which are based on disentangled attention with relative positional encoding, and they outperform state-of-the-art models in **13** out of 19 benchmarking tasks.

Additionally, we explored:
- Domain-specific pre-training
- Token representation strategies
- Fine-tuning methods

Our findings show that:
- Using relevant pre-training data significantly boosts performance
- Alternative pooling techniques can enhance classification
- There is a trade-off between efficiency and accuracy between full and parameter-efficient fine-tuning

These insights provide a foundation for optimizing model design and training in biological research.

## Features
In this repository, we include code for the following tasks:
- [Pre-training](https://github.com/omicsEye/seqLens/tree/main/train)
- [Benchmarking](https://github.com/omicsEye/seqLens/tree/main/benchmarking)
- [Visualization](https://github.com/omicsEye/seqLens/tree/main/visualizations)
- [Different pooling techniques for classification](https://github.com/omicsEye/seqLens/tree/main/classification_heads)
- [Vector representations for DNA sequences](https://github.com/omicsEye/seqLens/tree/main/vector_representation)

## Benchmarking Results
![Benchmarking Results](https://github.com/omicsEye/seqLens/blob/main/visualizations/plots/png/nt_esm_vs_deberta.png)

The following visualization shows how fine-tuning methods affect the performance of the models in vector representations:
<p align="center">
  <img src="https://github.com/omicsEye/seqLens/blob/main/visualizations/plots/gif/fourkingdoms_cls.gif" alt="Fine-tuning Effect" width="45%">
  <img src="https://github.com/omicsEye/seqLens/blob/main/visualizations/plots/gif/plasmids_cls.gif" alt="Fine-tuning Effect" width="45%">
</p>


## Usage
You can use these models for your research or use the provided scripts to train your models.

```python
# Load model directly
from transformers import AutoTokenizer, AutoModelForMaskedLM

tokenizer = AutoTokenizer.from_pretrained("omicseye/seqLens_4096_512_89M-at-base")
model = AutoModelForMaskedLM.from_pretrained("omicseye/seqLens_4096_512_89M-at-base")
```
# 🔧 Training & Compute Summary

All `seqLens` models were trained on **NVIDIA Tesla V100 GPUs** using distributed training via **HuggingFace Accelerate**.

---

## 📊 Representative Training Configurations

| Model | Params | Vocab | Context | # GPUs | Batch / GPU | Effective Batch | Runtime (h) | GPU-hours | Avg TFLOPs |
|--------|--------|--------|---------|--------|-------------|-----------------|-------------|------------|------------|
| `seqLens_4096_512_15M` | 15M | 4096 | 512 | 2 | 128 | 256 | 43.97 | 87.9 | 15.56 |
| `seqLens_4096_512_47M-at` | 47M | 4096 | 512 | 2 | 64 | 128 | 50.11 | 100.2 | 13.76 |
| `seqLens_4096_512_89M-at-base` | 89M | 4096 | 512 | 4 | 16 | 64 | 167.98 | 671.9 | 5.55 |
| `seqLens_4096_512_89M-at-base-multi` | 89M | 4096 | 512 | 1 | 64 | 64 | 507.61 | 507.6 | 6.02 |

> **GPU-hours = runtime × number of GPUs**

---

## 🖥 Hardware Utilization Distribution

| # GPUs Used | Number of Runs | Typical Models |
|--------------|----------------|----------------|
| 1 GPU | 1 run | 89M-at-base-multi |
| 2 GPUs | Majority of runs | 15M, 23M, 46M, 47M |
| 4 GPUs | Several runs | 23M-at-xsmall, 47M-at-small, 89M-at-base |

### 🔍 Observations

- Most experiments were conducted using **2× V100 GPUs**
- Larger models (≥ 89M parameters) benefit from **4 GPUs**
- Single-GPU training is feasible but increases wall-clock time
- TFLOPs decrease for larger models due to memory and communication overhead

---

## ⚙️ Training Characteristics

- Context length: **512 tokens** (~2.8–3kb DNA sequence context)
- Vocabulary size: **4096 (BPE tokenizer)**
- Mixed precision enabled
- Gradient checkpointing enabled for larger models
- Typical training schedule: **up to 150K steps**
- Runtime and TFLOPs logged per iteration

---

## 💡 Reproducibility Guidance

| Model Size | Recommended Hardware | Expected Runtime |
|-------------|---------------------|------------------|
| 15M–47M | 2× V100 GPUs | ~40–60 hours |
| 89M | 4× V100 GPUs | ~160–170 hours |
| 89M (single GPU) | 1× V100 GPU | ~500 hours |

---

## 📦 Environment

- GPU: NVIDIA Tesla V100
- Framework: PyTorch
- Distributed Training: HuggingFace Accelerate
- Logging: TFLOPs, runtime, and iteration time tracked per step
## Citation
If you use `seqLens` in your research, please cite our work:
```
@article{Baghbanzadeh2026,
  title = {seqLens: Optimizing Language Models for Genomic Predictions},
  volume = {43},
  ISSN = {1537-1719},
  url = {http://dx.doi.org/10.1093/molbev/msag139},
  DOI = {10.1093/molbev/msag139},
  number = {7},
  journal = {Molecular Biology and Evolution},
  publisher = {Oxford University Press (OUP)},
  author = {Baghbanzadeh,  Mahdi and Mann,  Brendan T and Crandall,  Keith A and Rahnavard,  Ali},
  editor = {Tamura,  Koichiro},
  year = {2026},
  month = June 
}
```

## License
This project is licensed under the Creative Commons Attribution-NonCommercial 4.0 International (CC-BY-NC 4.0) License.  
Commercial use of this software or any related models may require a separate licensing agreement due to a pending patent.  
For commercial inquiries, please contact Ali Rahnavard at rahnavard@gwu.edu.

## Contact
For any questions, feel free to email or open an issue in this repository.

