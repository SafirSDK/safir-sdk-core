using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Sate
{
    public partial class FilterControl : UserControl
    {
        public delegate void FilterHandler(string filter);

        public event FilterHandler FilterChanged;

        public FilterControl()
        {
            InitializeComponent();
            this.filterText.TextChanged += onFilterChanged;
        }

        private void onFilterChanged(object sender, EventArgs e)
        {
            FilterChanged?.Invoke(filterText.Text);
        }

        private void clearButton_Click(object sender, EventArgs e)
        {
            this.filterText.Text = string.Empty;
        }
    }
}
