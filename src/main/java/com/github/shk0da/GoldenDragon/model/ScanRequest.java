package com.github.shk0da.GoldenDragon.model;

import java.util.List;

public class ScanRequest {

    private List<Filter> filter;
    private Options options;
    private Symbols symbols;
    private List<String> columns;
    private Sort sort;
    private int[] range = new int[]{0, 100};

    public ScanRequest() {
    }

    public ScanRequest(List<Filter> filter, Options options, List<String> columns, Sort sort, int[] range) {
        this.filter = filter;
        this.options = options;
        this.columns = columns;
        this.sort = sort;
        this.range = range;
    }

    public ScanRequest(List<Filter> filter, Options options, Symbols symbols, List<String> columns, Sort sort, int[] range) {
        this.filter = filter;
        this.options = options;
        this.symbols = symbols;
        this.columns = columns;
        this.sort = sort;
        this.range = range;
    }

    public static class Filter {

        public String left;
        public String operation;
        public Object right;

        public Filter(String left, String operation) {
            this.left = left;
            this.operation = operation;
        }

        public Filter(String left, String operation, Object right) {
            this.left = left;
            this.operation = operation;
            this.right = right;
        }
    }

    public static class Options {

        public String lang;

        public Options(String lang) {
            this.lang = lang;
        }
    }

    public static class Symbols {

        public Query query;
        public List<String> tickers;

        public static class Query {
            public List<String> types;
        }
    }

    public static class Sort {

        public String sortBy;
        public String sortOrder;

        public Sort(String sortBy, String sortOrder) {
            this.sortBy = sortBy;
            this.sortOrder = sortOrder;
        }
    }

    public List<Filter> getFilter() {
        return filter;
    }

    public void setFilter(List<Filter> filter) {
        this.filter = filter;
    }

    public Options getOptions() {
        return options;
    }

    public void setOptions(Options options) {
        this.options = options;
    }

    public Symbols getSymbols() {
        return symbols;
    }

    public void setSymbols(Symbols symbols) {
        this.symbols = symbols;
    }

    public List<String> getColumns() {
        return columns;
    }

    public void setColumns(List<String> columns) {
        this.columns = columns;
    }

    public Sort getSort() {
        return sort;
    }

    public void setSort(Sort sort) {
        this.sort = sort;
    }

    public int[] getRange() {
        return range;
    }

    public void setRange(int[] range) {
        this.range = range;
    }
}
