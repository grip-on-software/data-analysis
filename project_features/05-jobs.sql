SELECT ${f(join_cols, "jenkins")}, SUM(jobs) AS num_jobs
FROM gros.${t("jenkins")}
${g(join_cols, "jenkins")}
