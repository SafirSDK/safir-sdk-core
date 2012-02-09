#! /usr/bin/python
#
# Created by: Joel Ottosson / joot
#
# Simple gui for Linux bridge utils and netem
#

import subprocess
import commands
import time
import Tkinter

# Some values that shold be treated as constants
DEVICE_PATH = "/proc/net/dev"                   #here we find existing network adapters
BRIDGE_NAME = "br0"                             #name of the one and only network bridge
BRIDGE_FILTER_PATH = "/proc/sys/net/bridge"    #filters that we reset at start


# Get a list of all normal network adapters
def get_network_adapters():
    result = []
    device_file = open(DEVICE_PATH)
    for line in device_file:
        tokens=line.split(None, 1)
        if tokens[0].startswith("eth"):
            result.append(tokens[0][:-1]) #remove ending ':'
    return result

# Get bool if network adapter is enabled, True->"up", False->"down"
def get_adapter_enabled(name):
    enabled=False
    proc = subprocess.Popen(["ifconfig", "-s"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    for line in proc.stdout:
        if line.startswith(name):
            enabled=True
            break
    proc.wait()
    return enabled
 
# Set bridge to up or down, True = "up", False= "down"
def set_adapter_enabled(name, enabled):           
    if enabled:
        subprocess.call(["ifconfig", name, "up"])
    else:
        subprocess.call(["ifconfig", name, "down"])    
    
# Add list of network adapters to netem. We do this for all used adapters one time,
# then we can safely use 'change' instead of 'add' in all netem calls
def add_netem(ifs):
    for i in ifs:
        subprocess.call(["tc", "qdisc", "add", "dev", i, "root", "netem"]) #we just ignore error when already added
        
# Get the status string from 'tc qdisc show' for specific adapter
def get_adapter_netem_status(name):
    result="Normal"
    proc = subprocess.Popen(["tc", "qdisc", "show"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    for line in proc.stdout:
        ix=line.find(name)
        if ix!=-1:
            result=line[line.find("limit 1000", ix)+10:-1]
            break            
    if len(result)==0:
        return "Normal"
    return result

# Emulate packet delay
def set_netem_delay(name, val):
    subprocess.call(["tc", "qdisc", "change", "dev", name, "root", "netem", "delay", str(val)+"ms"])

# Emulate packet duplication    
def set_netem_duplicate(name, val):
    subprocess.call(["tc", "qdisc", "change", "dev", name, "root", "netem", "duplicate", str(val)+"%"])

# Emulate packet corruption    
def set_netem_corrupt(name, val):
    subprocess.call(["tc", "qdisc", "change", "dev", name, "root", "netem", "corrupt", str(val)+"%"])

# Emulate packet loss    
def set_netem_loss(name, val):
    subprocess.call(["tc", "qdisc", "change", "dev", name, "root", "netem", "loss", str(val)+"%"])

# Emulate packet reordering. Fixed correlation value to 50%, also fixed delay to 10 ms that can be
# changed afterwards by using set_netem_delay    
def set_netem_reorder(name, val):
    subprocess.call(["tc", "qdisc", "change", "dev", name, "root", "netem", "delay", "10ms", "reorder", str(val)+"%", "50%"])
    
# Check if bridge is created
def bridge_exists():
    device_file = open("/proc/net/dev")
    for line in device_file:
        tokens=line.split(None, 1)
        if tokens[0].startswith(BRIDGE_NAME):
            return True
    return False
    
# Create a bridge including all network interfaces in list ifs
def create_bridge(ifs):
    if bridge_exists():
        print("Bridge is already created!")
        return    
    subprocess.call(["brctl", "addbr", BRIDGE_NAME])
    for i in ifs:
        subprocess.call(["ifconfig", i, "0.0.0.0"])
        subprocess.call(["brctl", "addif", BRIDGE_NAME, i])
    set_adapter_enabled(BRIDGE_NAME, True)    
    #commands.getoutput is deprecated, replace with something better
    reset_command = "for f in "+BRIDGE_FILTER_PATH+"/bridge-*; do echo 0 > $f; done"
    commands.getoutput(reset_command)

# Delete the bridge
def delete_bridge():
    if bridge_exists():
        set_adapter_enabled(BRIDGE_NAME, False)
        subprocess.call(["brctl", "delbr", BRIDGE_NAME])

#-------------------------------------------
# Gui class, creates all gui components
#-------------------------------------------
class Gui:
    def __init__(self):        
        self.root = Tkinter.Tk()        
        self.root.title("Network Emulator")
        
        # Variables to be bound to widget values
        self.adapter = Tkinter.StringVar(self.root)
        self.delay = Tkinter.IntVar(self.root)
        self.loss=Tkinter.IntVar(self.root)
        self.corruption=Tkinter.IntVar(self.root)
        self.duplication=Tkinter.IntVar(self.root)
        self.reorder=Tkinter.IntVar(self.root)
        self.bridgeCreated=Tkinter.IntVar(self.root)
        
        self.adapter_status = {} #dictionary intended to store key=adapter name, value=tuple(upDownCheckboxValue, statusLabelText)
                
        ifs=get_network_adapters() #get a list of all netword adapters
        add_netem(ifs) #add all to netem so we dont have to bother about if to use 'add' or 'change' later

        #netem LabelFrame
        set_group = Tkinter.LabelFrame(self.root, text="Netem", padx=5, pady=5)    
        set_group.grid(row=0, column=0, padx=10, pady=10, sticky=Tkinter.W)
        #select adapter
        Tkinter.Label(set_group, text="Network adapter").grid(row=0, column=0, sticky=Tkinter.W)
        all_ifs=["All"]+ifs
        self.adapter.set(all_ifs[0]) # default value
        w = apply(Tkinter.OptionMenu, (set_group, self.adapter) + tuple(all_ifs))
        w.grid(row=0, column=1, sticky=Tkinter.W)
        #delay
        Tkinter.Label(set_group, text="Delay(ms)").grid(row=1, column=0, sticky=Tkinter.W)
        Tkinter.Scale(set_group, orient=Tkinter.HORIZONTAL, variable=self.delay).grid(row=1, column=1, sticky=Tkinter.W)            
        Tkinter.Button(set_group, text="Set", command=self.on_set_delay).grid(row=1, column=2, sticky=Tkinter.W)
        #packet loss
        Tkinter.Label(set_group, text="Loss(%)").grid(row=2, column=0, sticky=Tkinter.W)
        Tkinter.Scale(set_group, orient=Tkinter.HORIZONTAL, variable=self.loss).grid(row=2, column=1, sticky=Tkinter.W)    
        Tkinter.Button(set_group, text="Set", command=self.on_set_loss).grid(row=2, column=2, sticky=Tkinter.W)
        #packet corruption
        Tkinter.Label(set_group, text="Corruption(%)").grid(row=3, column=0, sticky=Tkinter.W)
        Tkinter.Scale(set_group, orient=Tkinter.HORIZONTAL, variable=self.corruption).grid(row=3, column=1, sticky=Tkinter.W)    
        Tkinter.Button(set_group, text="Set", command=self.on_set_corruption).grid(row=3, column=2, sticky=Tkinter.W)
        #packet duplication
        Tkinter.Label(set_group, text="Duplication(%)").grid(row=4, column=0, sticky=Tkinter.W)
        Tkinter.Scale(set_group, orient=Tkinter.HORIZONTAL, variable=self.duplication).grid(row=4, column=1, sticky=Tkinter.W)    
        Tkinter.Button(set_group, text="Set", command=self.on_set_duplication).grid(row=4, column=2, sticky=Tkinter.W)
        #packet reordering
        Tkinter.Label(set_group, text="Reorder(%)").grid(row=5, column=0, sticky=Tkinter.W)
        Tkinter.Scale(set_group, orient=Tkinter.HORIZONTAL, variable=self.reorder).grid(row=5, column=1, sticky=Tkinter.W)    
        Tkinter.Button(set_group, text="Set", command=self.on_set_reorder).grid(row=5, column=2, sticky=Tkinter.W)

        #netem LabelFrame
        status_group = Tkinter.LabelFrame(self.root, text="Adapter status", padx=5, pady=5)    
        status_group.grid(row=1, column=0, padx=10, pady=10, sticky=Tkinter.W)
    
        #Bridge network checkbutton
        Tkinter.Checkbutton(status_group, 
                            text="Bridge adapters", 
                            variable=self.bridgeCreated, 
                            command=self.on_create_bridge).grid(row=0, column=0, sticky=Tkinter.W)
        self.adapter_status[BRIDGE_NAME]=(Tkinter.IntVar(self.root), Tkinter.StringVar(self.root))
        Tkinter.Checkbutton(status_group, 
                            text="bridge up", 
                            variable=self.adapter_status[BRIDGE_NAME][0], 
                            command=self.on_adapter_onoff).grid(row=0, column=1, sticky=Tkinter.W)        
        Tkinter.Frame(status_group, height=10).grid(row=1, columnspan=2)

        r=2
        for i in ifs:                            
            self.adapter_status[i]=(Tkinter.IntVar(self.root), Tkinter.StringVar(self.root))
            Tkinter.Checkbutton(status_group, 
                                text=i, 
                                variable=self.adapter_status[i][0], 
                                command=self.on_adapter_onoff).grid(row=r, column=0, sticky=Tkinter.W)                        
            Tkinter.Label(status_group, textvariable=self.adapter_status[i][1], bg="black", fg="yellow").grid(row=r, column=1, sticky=Tkinter.W)            
            r+=1            

        self.update_status()
    
    # For convenience, dont have to duplicate this code in all on_set_xxxx
    def set_netem_value(self, adapter, val, fun):
        if adapter!="All":
            fun(adapter, val)
        else:
            for i in get_network_adapters():
                fun(i, val)
        self.update_status()

    # Event handler methods on_xxxxxx, hanles gui widget input
    def on_set_delay(self):
        self.set_netem_value(self.adapter.get(), self.delay.get(), set_netem_delay)        
        
    def on_set_loss(self):
        self.set_netem_value(self.adapter.get(), self.loss.get(), set_netem_loss)
        
    def on_set_corruption(self):
        self.set_netem_value(self.adapter.get(), self.corruption.get(), set_netem_corrupt)
        
    def on_set_duplication(self):
        self.set_netem_value(self.adapter.get(), self.duplication.get(), set_netem_duplicate)

    def on_set_reorder(self):
        self.set_netem_value(self.adapter.get(), self.reorder.get(), set_netem_reorder)

    def on_create_bridge(self):
        if not bridge_exists():
            ifs=get_network_adapters()
            create_bridge(ifs)
        else:
            delete_bridge()
        self.update_status()
   
    def on_adapter_onoff(self):
        for k, v in self.adapter_status.items():
            current=v[0].get()
            if get_adapter_enabled(k)!=current:                
                set_adapter_enabled(k, current)                         
        self.update_status() 
        
    # Update status and checkboxes according to real netem/ifconfig values
    def update_status(self):
        self.bridgeCreated.set(bridge_exists())        
        for k, v in self.adapter_status.items():            
            v[0].set(get_adapter_enabled(k))
            v[1].set(get_adapter_netem_status(k))       
    
    # Run the main window loop
    def run(self):
        self.root.mainloop()
        return 0

# If this is the main module, start the program
if __name__ == "__main__":   
    g=Gui()
    g.run()       
    
