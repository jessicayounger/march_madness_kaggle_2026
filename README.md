# march_madness_kaggle_2026
Ratings-based prediction model for the Kaggle March Machine Learning Mania 2026 competition using Bart Torvik efficiency ratings and logistic calibration.

This repository contains a ratings-based model for the Kaggle March Machine Learning Mania 2026 competition.

Model overview
- Team strength: Bart Torvik adjusted efficiency ratings
- Probability model: logistic regression calibrated on historical NCAA games
- Calibration seasons:
  - Men: 2008–2025
  - Women: 2010–2025
- Game variance parameter: σ = 2

How to reproduce
1. Download Kaggle competition data
2. Place competition data files + Barttorvik Away-Neutral in /data
3. Run scripts/kaggle_submission_script.R
4. The script outputs output/submission.csv
