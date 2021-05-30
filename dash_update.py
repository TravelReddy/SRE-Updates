import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_table
import plotly.express as px
import pandas as pd
from datetime import date, datetime,timedelta
import numpy as np

app = dash.Dash(__name__)

df = pd.read_csv('labor_rate.csv')


app.layout = html.Div([
    #   html.H4('Dashboard'),
      dcc.Interval('graph-update', interval = 2000, n_intervals = 0),

      html.Div([
    html.Div([dcc.Dropdown(id="crossfilter-yaxis-column",
                        options = [ {'label': i, 'value': i} for i in df.columns],
                        value='Rate',
                        multi=True)],
                        style={'width': '24%', 'display': 'inline-block'}),

    html.Div([dcc.Dropdown(id="crossfilter-xaxis-column",
                        options = [ {'label': i, 'value': i} for i in df.columns],
                        value='Time in',
                        multi=True)],
                        style={'width': '24%', 'display': 'inline-block'}),
    
    html.Div([dcc.Dropdown(id="crossfilter-yaxis2-column",
                        options = [ {'label': i, 'value': i} for i in df.columns],
                        value='Gross Rate',
                        multi=True)],
                        style={'width': '24%', 'display': 'inline-block'}),

    html.Div([dcc.Dropdown(id="crossfilter-xaxis2-column",
                        options = [ {'label': i, 'value': i} for i in df.columns],
                        value='Time in',
                        multi=True)],
                        style={'width': '24%', 'display': 'inline-block'}),

],style={
        'borderBottom': 'thin lightgrey solid',
        'backgroundColor': 'rgb(250, 250, 250)',
        'padding': '10px 5px'
    }),


        html.Div([
            dcc.Graph(id='crossfilter-indicator-scatter',style={'display': 'inline-block','width': '49%'}),
            dcc.Graph(id='crossfilter-indicator-line',style={'display': 'inline-block','width': '49%'})],
                    ),
    #   dash_table.DataTable(
    #       id = 'table',
    #       data = df.to_dict('records'),
    #       columns=[{"name": i, "id": i} for i in df.columns])
          ])

@app.callback(
        dash.dependencies.Output('crossfilter-indicator-scatter','figure'),
        [dash.dependencies.Input('crossfilter-xaxis-column','value'),
        dash.dependencies.Input('crossfilter-yaxis-column','value'),
        dash.dependencies.Input('graph-update', 'n_intervals')])

def updateTable(xaxis_column_name,yaxis_column_name,n):
    df = pd.read_csv('labor_rate.csv')
    print(df['Rate'][0])
    fig = px.scatter(df,x=xaxis_column_name,y=yaxis_column_name,template="simple_white")
    return fig
    # return df.to_dict('records')
    # return None

@app.callback(
    dash.dependencies.Output('crossfilter-indicator-line','figure'),
    [dash.dependencies.Input('crossfilter-yaxis2-column','value'),
    dash.dependencies.Input('crossfilter-xaxis2-column','value'),
    dash.dependencies.Input('graph-update', 'n_intervals')]
    
)
def update_graph2(yaxis_column_name,xaxis_column_name,n):
    df = pd.read_csv('labor_rate.csv')
    print(df['Gross Rate'][0])
    fig = px.line(data_frame=df,x=xaxis_column_name,y=yaxis_column_name,template="plotly")
    return fig



if __name__ == '__main__':
     app.run_server(debug=True, port=8050)