CREATE TABLE IF NOT EXISTS asset_period(
    asset TEXT NOT NULL,
    stockbroker TEXT NOT NULL,

    begin_date TEXT NOT NULL,
    end_date TEXT NOT NULL,

    previous_position_quantity INTEGER NOT NULL,
    previous_position_average_price NUMERIC NOT NULL,
    previous_position_average_cost NUMERIC NOT NULL,

    event_effect_type TEXT NULL,

    event_set_position_quantity INTEGER NOT NULL,
    event_set_position_average_price NUMERIC NOT NULL,
    event_set_position_average_cost NUMERIC NOT NULL,

    event_increase_quantity INTEGER NOT NULL,
    event_increase_average_price NUMERIC NOT NULL,
    event_increase_average_cost NUMERIC NOT NULL,

    event_decrease_quantity INTEGER NOT NULL,
    event_decrease_average_price NUMERIC NOT NULL,
    event_decrease_average_cost NUMERIC NOT NULL,

    purchase_quantity INTEGER NOT NULL,
    purchase_average_price NUMERIC NOT NULL,
    purchase_average_cost NUMERIC NOT NULL,

    sale_quantity INTEGER NOT NULL,
    sale_average_price NUMERIC NOT NULL,
    sale_average_cost NUMERIC NOT NULL,

    exercised_quantity NUMERIC NOT NULL,

    resulting_position_quantity INTEGER NOT NULL,
    resulting_position_average_price NUMERIC NOT NULL,
    resulting_position_average_cost NUMERIC NOT NULL,

    converted_to_asset TEXT NULL,
    converted_to_quantity NUMERIC NULL,

    PRIMARY KEY (begin_date, asset, stockbroker)
);

CREATE TABLE IF NOT EXISTS portfolio_asset_date_range(
    portfolio_name TEXT NOT NULL,

    asset TEXT NOT NULL,
    stockbroker TEXT NOT NULL,

    begin_date TEXT NOT NULL,
    end_date TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS portfolio_asset_date_range_name
    ON portfolio_asset_date_range(portfolio_name);

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

CREATE TABLE IF NOT EXISTS non_quote_date(
    date TEXT PRIMARY KEY
);
