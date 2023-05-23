using System;
using System.Windows.Forms;
using System.Linq;

namespace Sate
{
    public partial class PaginationControl : UserControl
    {
        public static readonly int[] PageSizes = new int[] {10, 25, 50, 100};

        public delegate void PaginationChangeEventHandler(int currentPage, int startIndex, int endIndex);
        public event PaginationChangeEventHandler PaginationChange;

        private int numberOfValues = 0;

        public PaginationControl(int numberOfValues)
        {
            InitializeComponent();
            itemsPerPage.Items.Clear();
            itemsPerPage.Items.AddRange(PageSizes.Select(s => s.ToString()).ToArray());
            //this.numberOfValues = numberOfValues;
            this.itemsPerPage.SelectedIndex = 0;
            NumberOfValues = numberOfValues;
        }

        public int NumberOfValues
        {
            get { return numberOfValues; }
            set
            {
                if (numberOfValues != value)
                {
                    numberOfValues = value;
                    UpdateNumberOfPages();
                    FirePaginationChange();
                }
            }
        }

        public int PageSize { get; private set; } = 10;

        public void Update(int numberOfValues, int showPageWithItemIndex)
        {
            this.numberOfValues = numberOfValues;
            UpdateNumberOfPages();
            currentPage.Value = Math.Max(1, Math.Min(currentPage.Maximum, 1 + showPageWithItemIndex / PageSize));
        }

        private void currentPage_ValueChanged(object sender, EventArgs e)
        {
            FirePaginationChange();
        }

        private void UpdateNumberOfPages()
        {
            var numberOfPages = Math.Max(1, (numberOfValues / PageSize) + (numberOfValues % PageSize > 0 ? 1 : 0));
            if (numberOfPages != currentPage.Maximum)
            {
                currentPage.Maximum = numberOfPages;
                totalNumberOfPagesLabel.Text = $"of {numberOfPages}";
            }
        }

        private void FirePaginationChange()
        {
            if (PaginationChange != null)
            {   
                var startIndex = PageSize * ((int)currentPage.Value - 1);
                var showCount = (int)Math.Min(numberOfValues - startIndex, PageSize);
                var endIndex = startIndex + showCount;
                PaginationChange((int)currentPage.Value, startIndex, endIndex);
            }
        }

        private void itemsPerPage_SelectedIndexChanged(object sender, EventArgs e)
        {
            var val = int.Parse(itemsPerPage.SelectedItem as string);
            if (PageSize != val)
            {
                PageSize = val;
                UpdateNumberOfPages();
                FirePaginationChange();
            }
        }
    }
}
