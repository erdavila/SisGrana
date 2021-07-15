CREATE TABLE IF NOT EXISTS asset_change(
    asset TEXT NOT NULL,
    stockbroker TEXT NOT NULL,
    date TEXT NOT NULL,

    by_event BOOLEAN NOT NULL,

    previous_position_quantity INTEGER NOT NULL,
    previous_position_average_price NUMERIC NOT NULL,
    previous_position_average_cost NUMERIC NOT NULL,

    event_trade_result_quantity INTEGER NOT NULL,
    event_trade_result_total_gross_value INTEGER NOT NULL,
    event_trade_result_total_cost INTEGER NOT NULL,

    post_event_position_quantity INTEGER NOT NULL,
    post_event_position_average_price NUMERIC NOT NULL,
    post_event_position_average_cost NUMERIC NOT NULL,

    purchase_quantity INTEGER NOT NULL,
    purchase_average_price NUMERIC NOT NULL,
    purchase_average_cost NUMERIC NOT NULL,

    sale_quantity INTEGER NOT NULL,
    sale_average_price NUMERIC NOT NULL,
    sale_average_cost NUMERIC NOT NULL,

    day_trade_result_quantity INTEGER NOT NULL,
    day_trade_result_total_gross_value INTEGER NOT NULL,
    day_trade_result_total_cost INTEGER NOT NULL,

    non_day_trade_operations_quantity INTEGER NOT NULL,
    non_day_trade_operations_average_price NUMERIC NOT NULL,
    non_day_trade_operations_average_cost NUMERIC NOT NULL,

    operations_trade_result_quantity INTEGER NOT NULL,
    operations_trade_result_total_gross_value INTEGER NOT NULL,
    operations_trade_result_total_cost INTEGER NOT NULL,

    resulting_position_quantity INTEGER NOT NULL,
    resulting_position_average_price NUMERIC NOT NULL,
    resulting_position_average_cost NUMERIC NOT NULL,

    PRIMARY KEY (date, asset, stockbroker)
);

CREATE TABLE IF NOT EXISTS asset_quote(
    asset TEXT NOT NULL,
    date TEXT NOT NULL,

    open_price NUMERIC NOT NULL,
    close_price NUMERIC NOT NULL,
    min_price NUMERIC NOT NULL,
    avg_price NUMERIC NOT NULL,
    max_price NUMERIC NOT NULL,

    PRIMARY KEY (asset, date)
);
