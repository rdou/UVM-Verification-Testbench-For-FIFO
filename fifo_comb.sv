import uvm_pkg::*;
`include "uvm_macros.svh"
`timescale 1ns/10ps

/***********************************************
* interface 
***********************************************/
interface dut_if;
    logic clock, reset, put, get, full_bar, empty_bar;
    logic [15:0] data_in;
    logic [15:0] data_out;
endinterface : dut_if

/***********************************************
* DUT
***********************************************/
module FIFO (
              clk, 
              reset, 
              put, 
              get, 
              data_in, 
              empty_bar, 
              full_bar, 
              data_out
);

parameter DEPTH = 8;
parameter WIDTH = 16;
parameter POINTER_WIDTH = 3;

input clk;
input reset;
input put;
input get;
input [WIDTH-1:0] data_in;

output empty_bar;
output full_bar;
output [WIDTH-1:0] data_out;

reg [POINTER_WIDTH:0] wr_ptr;
reg [POINTER_WIDTH:0] rd_ptr;
reg empty_bar;
reg full_bar;
reg [WIDTH-1:0] data_out;
reg [WIDTH-1:0] FIFO_3 [DEPTH-1:0];

wire [POINTER_WIDTH-1:0] wr_ptr_int;
wire [POINTER_WIDTH-1:0] rd_ptr_int;
wire full;
wire empty;
wire [POINTER_WIDTH:0] wr_rd;
wire put_e;
wire get_e;

assign wr_rd = wr_ptr - rd_ptr;
assign full  = (wr_rd == 4'b1000);
assign empty = (wr_rd == 4'b0000);
assign put_e = (put && full == 1'b0);
assign get_e = (get && empty == 1'b0);
assign wr_ptr_int = wr_ptr[POINTER_WIDTH-1:0];
assign rd_ptr_int = rd_ptr[POINTER_WIDTH-1:0];

always @(posedge clk)
begin
	if (reset)
	begin
		wr_ptr <= 3'b000;
		rd_ptr <= 3'b000;
	end else
	begin
		if (put_e) 
		begin
			FIFO_3 [wr_ptr_int] <= data_in;
			wr_ptr <= wr_ptr + 3'b001;
		end
		if (get_e)
		begin
			rd_ptr <= rd_ptr + 3'b001;
		end
	end
end 

always @(rd_ptr_int)
begin
	data_out <= FIFO_3 [rd_ptr_int-3'b001];
end

always @(empty or full)
begin
	empty_bar <= ~empty;
	full_bar <= ~full;
end 
endmodule : FIFO

/***********************************************
* top 
***********************************************/
module top;
	dut_if inf();

	FIFO dut (
			.clk(inf.clock),
			.reset(inf.reset),
			.put(inf.put),
			.get(inf.get),
			.full_bar(inf.full_bar),
			.empty_bar(inf.empty_bar),
			.data_in(inf.data_in),
			.data_out(inf.data_out)
			);

	initial 
	begin
		uvm_config_db #(virtual dut_if)::set(null, "uvm_test_top.env_h.agent_1_h.data_driver_h", "dut_if", inf);
		uvm_config_db #(virtual dut_if)::set(null, "uvm_test_top.env_h.agent_2_h.rst_driver_h", "dut_if", inf);
		uvm_config_db #(virtual dut_if)::set(null, "uvm_test_top.env_h.agent_1_h.monitor_1_h", "dut_if", inf);
        uvm_config_db #(virtual dut_if)::set(null, "uvm_test_top.env_h.agent_3_h.monitor_2_h", "dut_if", inf);
        run_test("test_1");
	end

	always 
		#5 inf.clock = !inf.clock;
		
	initial 
	begin
		inf.clock = 1'b1;
		inf.reset = 1'b1;
		repeat (3) @(posedge inf.clock);
		#5
		inf.reset = 1'b0;
	end
endmodule : top

/***********************************************
* transaction 
***********************************************/
class data_transaction extends uvm_sequence_item;
    rand logic [15 : 0] data_in;
    rand logic          put;
    rand logic          get; 

  function new(string name = "data_transaction");
        super.new(name);
    endfunction

    `uvm_object_utils_begin(data_transaction)
        `uvm_field_int(data_in, UVM_ALL_ON)
        `uvm_field_int(put, UVM_ALL_ON)
        `uvm_field_int(get, UVM_ALL_ON)
    `uvm_object_utils_end
endclass 

class rst_transaction extends uvm_sequence_item;
    logic rst;

  function new(string name = "rst_transaction");
        super.new(name);
    endfunction

    `uvm_object_utils_begin(rst_transaction)
        `uvm_field_int(rst, UVM_ALL_ON)
    `uvm_object_utils_end
endclass

/***********************************************
* sequence 
***********************************************/
// FIFO test 1
// write FIFO from empty to full -> read FIFO from full to empty
class FIFO_test_1_sequence extends uvm_sequence #(data_transaction); 
    `uvm_object_utils(FIFO_test_1_sequence)
    
    data_transaction tr;

  function new(string name = "data_seq_1");
        super.new(name); 
    endfunction
    
    task body();
        for (int i = 0; i < 40; i++) begin 
            tr = data_transaction::type_id::create("tx_data_tr");
            start_item(tr);
            
            if (i < 20) begin
                if (!tr.randomize() with {tr.put == 1'b1; tr.get == 1'b0;}) begin
                    `uvm_error("Sequence", "Randomization failure for trasaction")
                end 
            end
            else begin
                if (!tr.randomize() with {tr.put == 1'b0; tr.get == 1'b1;}) begin
                    `uvm_error("Sequence", "Randomization failure for trasaction")
                end 
            end

            finish_item(tr); 
        end
    endtask
endclass : FIFO_test_1_sequence   

// Write FIFO from empty to half-full, and then read/write at the same time
class FIFO_test_2_sequence extends uvm_sequence #(data_transaction);
	`uvm_object_utils(FIFO_test_2_sequence)
	
    data_transaction tr;
	
    function new(string name = "data_seq_1");
		super.new(name);
	endfunction
	
	task body();
		for (int i = 0; i < 40; i++) begin 
          	tr = data_transaction::type_id::create("rd_tr");
            start_item(tr);
            
            if (i < 4) begin
                if (!tr.randomize() with {tr.put == 1'b1; tr.get == 1'b0;}) begin
                    `uvm_error("Sequence", "Randomization failure for trasaction")
                end 
            end
            else begin
                if (!tr.randomize() with {tr.put == 1'b1; tr.get == 1'b1;}) begin
                    `uvm_error("Sequence", "Randomization failure for trasaction")
                end 
            end

            finish_item(tr); 
        end
	endtask
endclass

class FIFO_rst_sequence extends uvm_sequence #(rst_transaction);
    `uvm_object_utils(FIFO_rst_sequence)
    
    rst_transaction tr; 
    
  function new(string name = "rst_seq");
        super.new(name); 
    endfunction

    task body();
        tr = rst_transaction::type_id::create("rst_tx_tr");
        start_item(tr); 
        tr.rst = 1'b1;
        finish_item(tr); 

        tr = rst_transaction::type_id::create("rst_tx_tr");
        start_item(tr); 
        tr.rst = 1'b0;
        finish_item(tr); 
    endtask   
endclass

/***********************************************
* sequencer 
***********************************************/
class rst_sequencer extends uvm_sequencer #(rst_transaction);
	`uvm_component_utils(rst_sequencer)
  
	function new(string name, uvm_component parent);
		super.new(name, parent);
	endfunction
endclass : rst_sequencer

class data_sequencer extends uvm_sequencer #(data_transaction);
	`uvm_component_utils(data_sequencer)
  
	function new(string name, uvm_component parent);
		super.new(name, parent);
	endfunction
endclass : data_sequencer

/***********************************************
* virtual sequence 
***********************************************/
class top_vseq_base extends uvm_sequence #(uvm_sequence_item);
    `uvm_object_utils(top_vseq_base)
    
    rst_sequencer rst_sqr_h;
    data_sequencer data_sqr_h;
    
    function new(string name = "top_vseq_base");
        super.new(name);
    endfunction 
endclass

class vseq_rst_data extends top_vseq_base;
    `uvm_object_utils(vseq_rst_data)
  
    FIFO_test_1_sequence fifo_data_seq_h;
    FIFO_rst_sequence fifo_rst_seq_h;

    function new(string name = "vseq_rst_data");
        super.new(name); 
    endfunction

    task body();
        fifo_data_seq_h = FIFO_test_1_sequence::type_id::create("fifo_data_seq_h");   
        fifo_rst_seq_h  = FIFO_rst_sequence::type_id::create("fifo_rst_seq_h");    

      fork
        fifo_rst_seq_h.start(rst_sqr_h);  
        fifo_data_seq_h.start(data_sqr_h); 
      join
    endtask
endclass : vseq_rst_data 

/***********************************************
* driver 
***********************************************/
class data_driver extends uvm_driver #(data_transaction);
    `uvm_component_utils(data_driver)

    virtual dut_if driver2dut; 
    data_transaction tr; 
    
    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction
   
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        if (!uvm_config_db #(virtual dut_if)::get(this, "", "dut_if", driver2dut))
            `uvm_info("DATA_DRIVER", "uvm_config_db::get failed!", UVM_HIGH)
    endfunction

    task reset_check();
        forever begin
            @(posedge driver2dut.clock);

            if (driver2dut.reset) begin
                driver2dut.put = 1'b0; 
                driver2dut.get = 1'b0;
            end
        end
    endtask

    task send_data();
        forever begin
            @(posedge driver2dut.clock); 
            
            if (!driver2dut.reset) begin 
                seq_item_port.get_next_item(tr); 
                #1
                driver2dut.put     = tr.put;
                driver2dut.get     = tr.get;
                driver2dut.data_in = tr.data_in;
                seq_item_port.item_done();
            end
        end
    endtask
    
    task run_phase(uvm_phase phase);
        fork
            reset_check();
            send_data();
        join 
    endtask 
endclass : data_driver

class rst_driver extends uvm_driver #(rst_transaction);
    `uvm_component_utils(rst_driver)
    
    rst_transaction tr; 
    virtual dut_if driver2dut;

    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction
    
    function void build_phase(uvm_phase phase);
        if (!uvm_config_db #(virtual dut_if)::get(this, "", "dut_if", driver2dut))
            `uvm_info("RST_DRIVER", "uvm_config_db::get failed!", UVM_HIGH)
    endfunction    
    
    task run_phase(uvm_phase phase);
        seq_item_port.get_next_item(tr); 
        driver2dut.reset = tr.rst; 
        repeat (3) @(negedge driver2dut.clock);
        seq_item_port.item_done();

        seq_item_port.get_next_item(tr); 
        driver2dut.reset = tr.rst; 
        seq_item_port.item_done();
    endtask
endclass : rst_driver

/***********************************************
* monitor 
***********************************************/
class my_monitor_1 extends uvm_monitor;
    `uvm_component_utils(my_monitor_1)
    
    virtual dut_if dut2monitor1;
  	uvm_analysis_port #(data_transaction) ap;
    data_transaction tr;

    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        ap = new("monitor_1_ap", this);
		if (!uvm_config_db #(virtual dut_if)::get(this, "", "dut_if", dut2monitor1))
            $display("monitor_1 uvm_config_db::get failed");
    endfunction

    task run_phase(uvm_phase phase);
	forever 
		begin
			@(posedge dut2monitor1.clock)	
			tr = data_transaction::type_id::create("tr");
			if (!dut2monitor1.reset & dut2monitor1.full_bar & dut2monitor1.put)
			begin
                tr.data_in = dut2monitor1.data_in;	
                ap.write(tr);
			end
		end 
    endtask
endclass : my_monitor_1

class my_monitor_2 extends uvm_monitor;
    `uvm_component_utils(my_monitor_2)
    
    virtual dut_if dut2monitor2;
  	uvm_analysis_port #(data_transaction) ap;
    data_transaction tr;

    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        ap = new("monitor_2_ap", this);
		if (!uvm_config_db #(virtual dut_if)::get(this, "", "dut_if", dut2monitor2))
            $display("monitor_2 uvm_config_db::get failed");
    endfunction

    task run_phase(uvm_phase phase);
	forever 
		begin
			@(posedge dut2monitor2.clock)	
			tr = data_transaction::type_id::create("tr");
			if (!dut2monitor2.reset & dut2monitor2.empty_bar & dut2monitor2.get)
			begin
                #1
                tr.data_in = dut2monitor2.data_out;	
                ap.write(tr);
			end
		end 
    endtask
endclass : my_monitor_2

/***********************************************
* agent 
***********************************************/
class my_agent_1 extends uvm_component;
    `uvm_component_utils(my_agent_1)
    
  	uvm_analysis_port #(data_transaction) ap; 
    data_driver data_driver_h; 
    data_sequencer data_sequencer_h;
    my_monitor_1 monitor_1_h;    
 
    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        data_driver_h     = data_driver::type_id::create("data_driver_h", this); 
        data_sequencer_h  = data_sequencer::type_id::create("data_sequencer_h", this); 
        monitor_1_h       = my_monitor_1::type_id::create("monitor_1_h", this);
    endfunction

    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        data_driver_h.seq_item_port.connect(data_sequencer_h.seq_item_export);
        ap = monitor_1_h.ap; 
    endfunction
endclass : my_agent_1 

class my_agent_2 extends uvm_component;
    `uvm_component_utils(my_agent_2)

    rst_driver rst_driver_h; 
    rst_sequencer rst_sequencer_h;
 
    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        rst_driver_h    = rst_driver::type_id::create("rst_driver_h", this);
        rst_sequencer_h = rst_sequencer::type_id::create("rst_sequencer_h", this); 
    endfunction

    function void connect_phase(uvm_phase phase);
        rst_driver_h.seq_item_port.connect(rst_sequencer_h.seq_item_export);
    endfunction 
endclass : my_agent_2
   
class my_agent_3 extends uvm_component;
    `uvm_component_utils(my_agent_3)
    
  	uvm_analysis_port #(data_transaction) ap; 
    my_monitor_2 monitor_2_h;    
    
    function new(string name, uvm_component parent);
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        monitor_2_h = my_monitor_2::type_id::create("monitor_2_h", this);
    endfunction

    function void connect_phase(uvm_phase phase);
        ap = monitor_2_h.ap; 
    endfunction
endclass : my_agent_3 

/***********************************************
* scoreboard 
***********************************************/
class my_scoreboard extends uvm_scoreboard;
	`uvm_component_utils(my_scoreboard);

  uvm_blocking_get_port #(data_transaction) exp_port;
  uvm_blocking_get_port #(data_transaction) act_port;	
	data_transaction tr_act, tr_exp;
	bit result;
	
	function new(string name, uvm_component parent);
		super.new(name, parent);
	endfunction	
	
	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		exp_port = new("exp_port", this);
		act_port = new("act_port", this);		
	endfunction
	
	task run_phase(uvm_phase phase);
		forever
		begin
			exp_port.get(tr_exp);
			act_port.get(tr_act);
			result = tr_exp.compare(tr_act);
			if (result)
				$display("Compare SUCCESSFULLY");
			else
            	`uvm_warning("WARNING", "Compare FAILED")
			$display("The expected data is");
			tr_exp.print();
			$display("The actual data is");
			tr_act.print();	
		end
	endtask		
endclass : my_scoreboard

/***********************************************
* environment 
***********************************************/
class my_environment extends uvm_env;
    `uvm_component_utils(my_environment);
    
    my_agent_1 agent_1_h; 
    my_agent_2 agent_2_h; 
    my_agent_3 agent_3_h; 
    my_scoreboard scoreboard_h; 
  uvm_tlm_analysis_fifo #(data_transaction) agt_1_scb_fifo;
  uvm_tlm_analysis_fifo #(data_transaction) agt_2_scb_fifo;
     
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
     	agent_1_h    = my_agent_1::type_id::create("agent_1_h", this);
		agent_2_h    = my_agent_2::type_id::create("agent_2_h", this);
		agent_3_h    = my_agent_3::type_id::create("agent_3_h", this);
		scoreboard_h = my_scoreboard::type_id::create("scoreboard_h", this);
		agt_1_scb_fifo = new("agt_1_scb_fifo", this);
		agt_2_scb_fifo = new("agt_2_scb_fifo", this);    
    endfunction

    function void connect_phase(uvm_phase phase);
		agent_1_h.ap.connect(agt_1_scb_fifo.analysis_export);
		agent_3_h.ap.connect(agt_2_scb_fifo.analysis_export);
		scoreboard_h.exp_port.connect(agt_1_scb_fifo.blocking_get_export);
		scoreboard_h.act_port.connect(agt_2_scb_fifo.blocking_get_export);
	endfunction
endclass : my_environment

/***********************************************
* test 
***********************************************/
// vsim top +UVM_TESTNAME=test_1
// vsim top +UVM_TESTNAME=test_2
// select different test cases



class top_test_base extends uvm_test;
    `uvm_component_utils(top_test_base)
    
    my_environment env_h;

    function new(string name = "top_test_base", uvm_component parent);   
        super.new(name, parent); 
    endfunction

    function void build_phase(uvm_phase phase);
        env_h = my_environment::type_id::create("env_h", this); 
    endfunction

    task init_vseq(top_vseq_base vseq);
        vseq.data_sqr_h = env_h.agent_1_h.data_sequencer_h;  
        vseq.rst_sqr_h  = env_h.agent_2_h.rst_sequencer_h; 
    endtask
endclass : top_test_base

class test_1 extends top_test_base;
    `uvm_component_utils(test_1)

	vseq_rst_data vseq_h;
  
  function new(string name = "test_1", uvm_component parent);   
        super.new(name, parent); 
    endfunction
    
    function void build_phase(uvm_phase phase);
      	super.build_phase(phase);
        vseq_h = vseq_rst_data::type_id::create("vseq_h"); 
    endfunction 

    task run_phase(uvm_phase phase);
        phase.raise_objection(this);
        init_vseq(vseq_h);
        vseq_h.start(null);
        phase.drop_objection(this);
    endtask    
endclass : test_1  

//class test_1 extends my_test_base;
//    `uvm_component_utils(test_1)    
//
//    my_environment environment_h;
//
//    function new(string name = "test_1", uvm_component parent = null);   
//        super.new(name, parent); 
//    endfunction
//    
//    function void build_phase(uvm_phase phase);
//        super.build_phase(phase);
//        environment_h = my_environment::type_id::create("env_h", this);
//        uvm_config_db #(uvm_object_wrapper)::set(this, "env_h.agent_1_h.sequencer_h.run_phase", 
//                                                 "default_sequence", FIFO_test_1_sequence::type_id::get());
//    endfunction
//
//    task run_phase(uvm_phase phase);
//    endtask 
//endclass : test_1
//
//class test_2 extends my_test_base;
//    `uvm_component_utils(test_2)    
//
//    my_environment environment_h;
//
//    function new(string name = "test_2", uvm_component parent = null);   
//        super.new(name, parent); 
//    endfunction
//    
//    function void build_phase(uvm_phase phase);
//        super.build_phase(phase);
//        environment_h = my_environment::type_id::create("env_h", this);
//      	uvm_config_db #(uvm_object_wrapper)::set(this, "env_h.agent_1_h.sequencer_h.run_phase", 
//                                                 "default_sequence", FIFO_test_2_sequence::type_id::get());
//    endfunction
//endclass : test_2

