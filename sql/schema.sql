CREATE TABLE IF NOT EXISTS asset_change(
    asset TEXT NOT NULL,
    stockbroker TEXT NOT NULL,
    date TEXT NOT NULL,

    by_event BOOLEAN NOT NULL,

    purchase_quantity INTEGER NOT NULL,
    purchase_total_value NUMERIC NOT NULL,
    purchase_cost_total NUMERIC NOT NULL,

    sale_quantity INTEGER NOT NULL,
    sale_total_value NUMERIC NOT NULL,
    sale_cost_total NUMERIC NOT NULL,

    resulting_quantity INTEGER NOT NULL,
    resulting_total_value NUMERIC NOT NULL,
    resulting_cost_total_value NUMERIC NOT NULL,

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
