SELECT ${f(join_cols, "issue")}, COUNT(DISTINCT issue_id) AS num_tests
FROM gros.${t("issue")}
WHERE ${s(issue_test_case)}
${g(join_cols, "issue")}
