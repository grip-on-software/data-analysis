SELECT project_id, COUNT(DISTINCT issue_id) AS num_tests
FROM gros.${t("issue")}
WHERE ${s(issue_test_case)} GROUP BY project_id
