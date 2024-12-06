import streamlit as st
import pandas as pd
import plotly.express as px

# File paths
file_paths = {
    "kroger_total_sales": "kroger_total_sales.csv",
    "kroger_total_private_label_sales": "kroger_total_private_label_sales.csv",
    "kroger_pl_penetration_rate": "Kroger_PL_Penetration_Rate.csv",
    "benchmark_comparison_gap": "Benchmark_Comparison_Gap.csv",
    "kroger_top_5_performing_categories": "kroger_top_5_performing_categories.csv",
    "income_bucket_chart": "income_bucket_chart.csv",
    "census_region_chart": "census_region_chart.csv",
    "age_bucket_chart": "age_bucket_chart.csv",
    "ethnicity_chart": "ethnicity_chart.csv",
    "top_region_opportunity": "top_region_opportunity.csv",
    "3_most_opportunistic_region": "3_most_opportunistic_region.csv",
    "top_age_bucket_opportunity": "top_age_bucket_opportunity.csv",
    "3_most_opportunistic_age_bucket": "3_most_opportunistic_age_bucket.csv",
    "top_ethnicity_opportunity": "top_ethnicity_opportunity.csv",
    "3_most_opportunistic_ethnicity": "3_most_opportunistic_ethnicity.csv",
    "numerator_logo": "Numerator Logo.png",
    "kroger_logo": "Kroger logo.png"
}


# Page layout
st.set_page_config(layout="wide", page_title="Private Label Opportunity Engine")

# Header
col1, col2 = st.columns([1, 8])
with col1:
    st.image(file_paths["numerator_logo"], width=100)
with col2:
    st.title("Private Label Opportunity Engine")

st.image(file_paths["kroger_logo"], width=150)

# Dropdown for retailer
st.sidebar.header("Select Retailer")
retailer = st.sidebar.selectbox("Retailer", ["Kroger"])


# KPI Section
st.header("KPIs")
kpi_col1, kpi_col2, kpi_col3, kpi_col4 = st.columns(4)

with kpi_col1:
    total_sales = pd.read_csv(file_paths["kroger_total_sales"])
    st.metric("Total Sales $", f"${total_sales['Total_Sales'].iloc[0]:,.2f}")

with kpi_col2:
    private_label_sales = pd.read_csv(file_paths["kroger_total_private_label_sales"])
    st.metric("Private Label Sales $", f"${private_label_sales['Total_Private_Label_Sales'].iloc[0]:,.2f}")

with kpi_col3:
    pl_penetration_rate = pd.read_csv(file_paths["kroger_pl_penetration_rate"])
    st.metric("Private Label Penetration Rate", f"{pl_penetration_rate['Penetration_Rate'].iloc[0] * 100:.2f}%")

with kpi_col4:
    comparison_gap = pd.read_csv(file_paths["benchmark_comparison_gap"])
    gap_value = comparison_gap['Gap'].iloc[0] * 100  # Assuming the Gap is a decimal (e.g., 0.05 for 5%)
    
    # Add "+" for positive values and "-" for negative values
    formatted_gap = f"{'+' if gap_value > 0 else ''}{gap_value:.2f}%"

    # Set color for positive (green) and negative (red)
    gap_color = "green" if gap_value > 0 else "red"
    
    # Display the title and metric value aligned with the others
    st.metric(label="Benchmark Comparison Gap", value=f"{formatted_gap}", delta=None, delta_color="inverse")




# Top 5 Performing Categories
st.header("Top 5 Performing Categories")
top_categories = pd.read_csv(file_paths["kroger_top_5_performing_categories"])
st.table(top_categories)

# Census Region Chart
st.header("Kroger vs Benchmark: Census Region")
census_chart = pd.read_csv(file_paths["census_region_chart"])
census_chart_melted = census_chart.melt(
    id_vars=["Census_Region"], 
    value_vars=["kroger_percentage", "benchmark_percentage"],
    var_name="Category", 
    value_name="Percentage"
)
census_chart_melted["Category"] = census_chart_melted["Category"].replace({
    "kroger_percentage": "Kroger",
    "benchmark_percentage": "Benchmark"
})
fig = px.bar(
    census_chart_melted,
    x="Census_Region",
    y="Percentage",
    color="Category",
    barmode="group",
    title="Census Region Comparison",
    labels={"Percentage": "Percentage (%)", "Census_Region": "Region", "Category": "Comparison"},
    color_discrete_map={"Kroger": "#003f5c", "Benchmark": "#7fe57f"}  # Dark blue for Kroger, Green for Benchmark
)
st.plotly_chart(fig, use_container_width=True)

# Age Bucket Chart
st.header("Kroger vs Benchmark: Age Bucket")
age_chart = pd.read_csv(file_paths["age_bucket_chart"])
age_chart_melted = age_chart.melt(
    id_vars=["AGE_BUCKET"], 
    value_vars=["kroger_percentage", "benchmark_percentage"],
    var_name="Category", 
    value_name="Percentage"
)
age_chart_melted["Category"] = age_chart_melted["Category"].replace({
    "kroger_percentage": "Kroger",
    "benchmark_percentage": "Benchmark"
})
fig = px.bar(
    age_chart_melted,
    x="AGE_BUCKET",
    y="Percentage",
    color="Category",
    barmode="group",
    title="Age Bucket Comparison",
    labels={"Percentage": "Percentage (%)", "AGE_BUCKET": "Age Bucket", "Category": "Comparison"},
    color_discrete_map={"Kroger": "#003f5c", "Benchmark": "#7fe57f"}  # Dark blue for Kroger, Green for Benchmark
)
st.plotly_chart(fig, use_container_width=True)

# Ethnicity Chart
st.header("Kroger vs Benchmark: Ethnicity")
ethnicity_chart = pd.read_csv(file_paths["ethnicity_chart"])
ethnicity_chart_melted = ethnicity_chart.melt(
    id_vars=["ETHNICITY"], 
    value_vars=["kroger_percentage", "benchmark_percentage"],
    var_name="Category",
    value_name="Percentage"
)
ethnicity_chart_melted["Category"] = ethnicity_chart_melted["Category"].replace({
    "kroger_percentage": "Kroger",
    "benchmark_percentage": "Benchmark"
})
fig = px.bar(
    ethnicity_chart_melted,
    x="ETHNICITY",
    y="Percentage",
    color="Category",
    barmode="group",
    title="Ethnicity Comparison",
    labels={"Percentage": "Percentage (%)", "ETHNICITY": "Ethnicity", "Category": "Comparison"},
    color_discrete_map={"Kroger": "#003f5c", "Benchmark": "#7fe57f"}  # Dark blue for Kroger, Green for Benchmark
)
st.plotly_chart(fig, use_container_width=True)

# Income Bucket Chart
st.header("Kroger vs Benchmark: Income Bucket")
income_chart = pd.read_csv(file_paths["income_bucket_chart"])
income_chart_melted = income_chart.melt(
    id_vars=["INCOME_BUCKET_LONG"], 
    value_vars=["kroger_percentage", "benchmark_percentage"],
    var_name="Category",
    value_name="Percentage"
)
income_chart_melted["Category"] = income_chart_melted["Category"].replace({
    "kroger_percentage": "Kroger",
    "benchmark_percentage": "Benchmark"
})
fig = px.bar(
    income_chart_melted,
    x="INCOME_BUCKET_LONG",
    y="Percentage",
    color="Category",
    barmode="group",
    title="Income Bucket Comparison",
    labels={"Percentage": "Percentage (%)", "INCOME_BUCKET_LONG": "Income Bucket", "Category": "Comparison"},
    color_discrete_map={"Kroger": "#003f5c", "Benchmark": "#7fe57f"}  # Dark blue for Kroger, Green for Benchmark
)
fig.update_layout(xaxis=dict(tickangle=45))  # Adjust x-axis labels for better readability
st.plotly_chart(fig, use_container_width=True)




# Opportunities by Region, Age Bucket, and Ethnicity
def display_opportunities(title, filepath, item_filepath):
    st.subheader(title)
    opportunity_data = pd.read_csv(filepath)
    st.table(opportunity_data)

    top_items = pd.read_csv(item_filepath)
    st.table(top_items)

display_opportunities("Top Census Region Opportunity", file_paths["top_region_opportunity"], file_paths["3_most_opportunistic_region"])
display_opportunities("Top Age Bucket Opportunity", file_paths["top_age_bucket_opportunity"], file_paths["3_most_opportunistic_age_bucket"])
display_opportunities("Top Ethnicity Opportunity", file_paths["top_ethnicity_opportunity"], file_paths["3_most_opportunistic_ethnicity"])







