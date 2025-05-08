# seqLens

This repository contains the code for the [`seqLens`](https://doi.org/10.1101/2025.03.12.642848) project, which is a study to build DNA language models.  
HF repo: [omicseye](https://huggingface.co/omicseye)

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

## Citation
If you use [`seqLens`](https://doi.org/10.1101/2025.03.12.642848) in your research, please cite our work:
```
@article {seqLens,
	author = {Baghbanzadeh, Mahdi and Mann, Brendan and Crandall, Keith A and Rahnavard, Ali},
	title = {seqLens: optimizing language models for genomic predictions},
	elocation-id = {2025.03.12.642848},
	year = {2025},
	doi = {10.1101/2025.03.12.642848},
	publisher = {Cold Spring Harbor Laboratory},
	URL = {https://www.biorxiv.org/content/early/2025/03/14/2025.03.12.642848},
	eprint = {https://www.biorxiv.org/content/early/2025/03/14/2025.03.12.642848.full.pdf},
	journal = {bioRxiv}
}
```

## License
This project is licensed under the Creative Commons Attribution-NonCommercial 4.0 International (CC-BY-NC 4.0) License.  
Commercial use of this software or any related models may require a separate licensing agreement due to a pending patent.  
For commercial inquiries, please contact Ali Rahnavard at rahnavard@gwu.edu.

## Contact
For any questions, feel free to email or open an issue in this repository.

