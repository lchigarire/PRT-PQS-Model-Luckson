README: PRT-PQS-Model-Luckson

Project Overview

Traditional risk portfolio monitoring often relies on aggregated summaries that obscure "hidden" multivariate risks. This project introduces a Portfolio Quality Score (PQS)—a unified metric designed to surface latent structural risks within Pension Risk Transfer (PRT) portfolios.

The Problem: Invisible Risk

In PRT transactions, insurers assume long-dated liabilities shaped by complex factors like longevity dynamics and benefit concentration. Standard dashboards often miss the structural risks that emerge at the subpopulation level.

The Solution: A Geometric Approach

This framework utilizes Unsupervised Learning to map these risks:
Feature Engineering: Extracts demographic, health, and socioeconomic features.
Dimensionality Reduction: Applies Principal Component Analysis (PCA) to identify the latent factors governing portfolio heterogeneity.
Clustering: Uses k-means clustering on PCA scores to segment the portfolio into distinct subpopulations based on risk profile.
The PQS Metric: Combines these components with an entropy-based concentration penalty to summarize structural risk into a single, interpretable score.

Key Results

Applied to a synthetic dataset of 20,000 annuitants, the model:
Identified three dominant latent factors explaining 45% of total variance.
Produced a PQS of 2.047, indicating moderate structural risk.
Demonstrated high robustness with an Adjusted Rand Index of 0.982 under validation.

Repository Structure

PCA Results.R: Script for extraction of latent risk factors.
Clustering Outcomes.R: Portfolio segmentation logic.
Model Sensitivity Shocks.R: Testing responses to longevity shocks.
